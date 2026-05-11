# ===========================================
# Tablas de frecuencia - proyecto LOXO
# MULTIMORBILIDAD 
# ===========================================

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(stringr)

# Lista de 60 enfermedades
diseases_list <- c(
  'ALLERGY', 'ANEMIA', 'ASTHMA', 'ATRIAL_FIBRILLATION', 'AUTOIMMUNE_DISEASES', 
  'BLINDNESS_VISUAL_IMPAIRMENT', 'BLOOD_AND_BLOOD_FORMING_ORGAN_DISEASES', 
  'BRADYCARDIAS_AND_CONDUCTION_DISEASES', 'CARDIAC_VALVE_DISEASES', 
  'CATARACT_AND_OTHER_LENS_DISEASES', 'CEREBROVASCULAR_DISEASE', 
  'CHROMOSOMAL_ABNORMALITIES', 'CHRONIC_INFECTIOUS_DISEASES', 
  'CHRONIC_KIDNEY_DISEASES', 'CHRONIC_LIVER_DISEASES', 
  'CHRONIC_PANCREAS_BILIARY_TRACT_AND_GALLBLADDER_DISEASES', 
  'CHRONIC_ULCER_OF_THE_SKIN', 'COLITIS_AND_RELATED_DISEASES', 
  'COPD_EMPHYSEMA_CHRONIC_BRONCHITIS', 'DEAFNESS_HEARING_IMPAIRMENT', 
  'DEMENTIA', 'DEPRESSION_AND_MOOD_DISEASES', 'DIABETES', 'DORSOPATHIES', 
  'DYSLIPIDEMIA', 'EAR_NOSE_THROAT_DISEASES', 'EPILEPSY', 
  'ESOPHAGUS_STOMACH_AND_DUODENUM_DISEASES', 'GLAUCOMA', 'HEART_FAILURE', 
  'HEMATOLOGICAL_NEOPLASMS', 'HYPERTENSION', 'INFLAMMATORY_ARTHROPATHIES', 
  'INFLAMMATORY_BOWEL_DISEASES', 'ISCHEMIC_HEART_DISEASE', 
  'MIGRAINE_AND_FACIAL_PAIN_SYNDROMES', 'MULTIPLE_SCLEROSIS', 
  'NEUROTIC_STRESS_RELATED_AND_SOMATOFORM_DISEASES', 'OBESITY', 
  'OSTEOARTHRITIS_AND_OTHER_DEGENERATIVE_JOINT_DISEASES', 'OSTEOPOROSIS', 
  'OTHER_CARDIOVASCULAR_DISEASES', 'OTHER_DIGESTIVE_DISEASES', 
  'OTHER_EYE_DISEASES', 'OTHER_GENITOURINARY_DISEASES', 
  'OTHER_METABOLIC_DISEASES', 'OTHER_MUSCULOSKELETAL_AND_JOINT_DISEASES', 
  'OTHER_NEUROLOGICAL_DISEASES', 'OTHER_PSYCHIATRIC_AND_BEHAVIORAL_DISEASES', 
  'OTHER_RESPIRATORY_DISEASES', 'OTHER_SKIN_DISEASES', 
  'PARKINSON_AND_PARKINSONISM', 'PERIPHERAL_NEUROPATHY', 
  'PERIPHERAL_VASCULAR_DISEASE', 'PROSTATE_DISEASES', 
  'SCHIZOPHRENIA_AND_DELUSIONAL_DISEASES', 'SLEEP_DISORDERS', 
  'SOLID_NEOPLASMS', 'THYROID_DISEASES', 'VENOUS_AND_LYMPHATIC_DISEASES'
)

# Asegurar orden alfabético estricto
diseases_list <- sort(diseases_list) 

calculate_combinations <- function(db_path, output_dir) {
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(dbDisconnect(con, shutdown = TRUE))
  
  tables <- dbListTables(con)
  # Identificar tablas independientemente de las mayúsculas
  pacientes_tbl_name <- tables[tolower(tables) == "pacientes"][1]
  enfermedades_tbl_name <- tables[tolower(tables) == "enfermedades"][1]
  
  if (is.na(pacientes_tbl_name) || is.na(enfermedades_tbl_name)) {
    stop("No se encontraron las tablas 'pacientes' o 'enfermedades' en la base de datos.")
  }
  
  pacientes_db <- tbl(con, pacientes_tbl_name)
  enfermedades_db <- tbl(con, enfermedades_tbl_name)
  
  # Calculamos Edad a inicio de cohorte
  pacientes_db <- pacientes_db %>%
    select(Id_pac, F_Nac, Sexo, CCAA) %>%
    collect() %>%
    mutate(
      Edad = 2012 - as.numeric(F_Nac),
      Ageband = case_when(
        Edad >= 60 & Edad <= 69 ~ "60-69",
        Edad >= 70 & Edad <= 79 ~ "70-79",
        Edad >= 80 & Edad <= 89 ~ "80-89",
        Edad >= 90 ~ "90",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Ageband))
  
  years <- c(2012, 2017, 2022)
  
  for (yr in years) {
    cutoff_date <- paste0(yr, "-01-01")
    
    # 1. Filtramos enfermedades antes de la fecha de corte
    enf_filtered <- enfermedades_db %>%
      filter(F_EnfC < cutoff_date) %>%
      select(Id_pac, EnfC) %>%
      distinct() %>%
      collect()
      
    # 2. Pivotar enfermedades a ancho
    enf_wide <- enf_filtered %>%
      mutate(value = 1) %>%
      pivot_wider(names_from = EnfC, values_from = value, values_fill = 0)
      
    # Añadir enfermedades ausentes en la cohorte actual para completar las 60
    missing_diseases <- setdiff(diseases_list, names(enf_wide))
    if(length(missing_diseases) > 0) {
      for(md in missing_diseases) {
        enf_wide[[md]] <- 0
      }
    }
    
    # Cruzar pacientes con enfermedades
    df_combined <- pacientes_db %>%
      left_join(enf_wide, by = "Id_pac")
      
    # Rellenar con 0 para pacientes sin enfermedades (o NA por left_join)
    df_combined <- df_combined %>%
      mutate(across(all_of(diseases_list), ~replace_na(.x, 0)))
      
    # Generar string de combinación binario (alfabético estricto)
    combo_str <- do.call(paste0, df_combined[diseases_list])
    df_combined$Combination <- combo_str
    
    # 3. Agrupar y contar frecuencias
    results <- df_combined %>%
      group_by(Sexo, Ageband, CCAA, Combination) %>%
      summarise(Frequency = n(), .groups = "drop") %>%
      mutate(Year = yr) %>%
      select(Year, Sexo, Ageband, CCAA, Combination, Frequency)
      
    # 4. Generar CSVs
    groups_to_save <- results %>% group_by(Sexo, Ageband, CCAA) %>% group_split()
    
    for(g in groups_to_save) {
      if(nrow(g) == 0) next
      
      cur_sexo <- unique(g$Sexo)[1]
      cur_age <- unique(g$Ageband)[1]
      cur_ccaa <- unique(g$CCAA)[1]
      
      # Limpiar variables para nombres de archivo seguros
      s_clean <- ifelse(is.na(cur_sexo), "NA", as.character(cur_sexo))
      a_clean <- ifelse(is.na(cur_age), "NA", as.character(cur_age))
      a_clean <- str_replace_all(a_clean, ">90", "over90") # Evitar caracteres especiales en OS
      c_clean <- ifelse(is.na(cur_ccaa), "NA", as.character(cur_ccaa))
      
      filename <- paste0("frequencies_", yr, "_", s_clean, "_", a_clean, "_", c_clean, ".csv")
      filepath <- file.path(output_dir, filename)
      
      write.csv(g, filepath, row.names = FALSE)
    }
  }
  
  message("Archivos generados exitosamente en ", output_dir)
}

# Ejecución desde terminal (opcional)
if (!interactive() && sys.nframe() == 0) {
  db_path <- "../../inputs/data.duckdb"
  out_path <- "../../outputs"
  
  if (file.exists(db_path)) {
    dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
    calculate_combinations(db_path, out_path)
  }
}
