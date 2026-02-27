#!/usr/bin/env python3
import logging
import time
from pathlib import Path

# ------------------------------
# Correct outputs folder
# ------------------------------
# Use the same outputs folder as the LOXO web app
PROJECT_ROOT = Path("/home/mambauser/projects/loxo")  # this is owned by $MAMBA_USER
OUTPUT_DIR = PROJECT_ROOT / "outputs"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)  # safe, writable by $MAMBA_USER

LOG_FILE = OUTPUT_DIR / "memory_execution.log"

# ------------------------------
# Configure logging
# ------------------------------
logging.basicConfig(
    filename=LOG_FILE,
    format="%(asctime)s | %(levelname)s | %(message)s",
    level=logging.INFO,
)

logging.info("Starting container memory logging...")

# ------------------------------
# Memory tracking
# ------------------------------
CGROUP_MEM_FILE = "/sys/fs/cgroup/memory.current"

try:
    while True:
        if Path(CGROUP_MEM_FILE).exists():
            with open(CGROUP_MEM_FILE) as f:
                mem_bytes = int(f.read())
                mem_mb = mem_bytes // 1024 // 1024
                logging.info(f"Memory usage: {mem_mb} MB")
        else:
            logging.warning(f"{CGROUP_MEM_FILE} not found. Cannot track memory.")
        time.sleep(15)
except KeyboardInterrupt:
    logging.info("Memory logging stopped by KeyboardInterrupt")
except Exception as e:
    logging.error(f"Memory logging error: {e}")
