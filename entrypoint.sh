#!/usr/bin/env bash
set -e

# Launch memory logger in background using the aspire environment
micromamba run -n aspire python /opt/memory_logger.py &

# Execute the main LOXO command
exec "$@"