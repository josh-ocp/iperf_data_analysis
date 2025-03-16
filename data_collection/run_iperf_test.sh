#!/bin/bash

# This script is designed to be run as root via Jamf policy

# Define defaults
DEFAULT_SERVERS=("ash.ocp.org" "sycamore.ocp.org")
PORT=5201
USERNAME=$(whoami)
DURATION=120  # Default to 120-second tests instead of -n 1G
INTERVAL=1    # Report every second

# Create results folder with proper standardized location
RESULTS_FOLDER="/var/log/iperf_data"
mkdir -p "$RESULTS_FOLDER"

# Get current date and time in the format expected by R parsing code
CURRENT_DATE=$(date +"%Y%m%d")
CURRENT_TIME=$(date +"%H%M%S")
CURRENT_DATETIME="${CURRENT_DATE}_${CURRENT_TIME}"

# Create log file name
LOG_FILE="${RESULTS_FOLDER}/connection_test.log"

# Log script start
echo "[$CURRENT_DATETIME] Starting silent iperf test script" >> "$LOG_FILE"

# Parse command-line options (keeping for flexibility but not expected in Jamf deployment)
SERVERS=()

while getopts "s:d:u:p:h" opt; do
  case $opt in
    s) SERVERS+=("$OPTARG") ;;
    d) DURATION=$OPTARG ;;
    u) USERNAME=$OPTARG ;;
    p) PORT=$OPTARG ;;
    h) exit 0 ;;
    *) exit 1 ;;
  esac
done

# If no servers specified, use defaults
if [ ${#SERVERS[@]} -eq 0 ]; then
    SERVERS=("${DEFAULT_SERVERS[@]}")
fi

# Check for required tools
for cmd in iperf3 whois curl; do
    if ! command -v $cmd &> /dev/null; then
        echo "[$CURRENT_DATETIME] Required command '$cmd' not found" >> "$LOG_FILE"
        exit 1
    fi
done

# Find ISP information using whois with fallback
IP=$(curl -s https://ipinfo.io/ip)
ISP=$(whois "$IP" | grep -i "OrgName" | awk -F: '{print $2}' | xargs)
if [[ -z "$ISP" ]]; then
    ISP="UnknownISP"
fi

# Sanitize ISP name for filename (replace spaces/special chars with underscores)
ISP_SAFE=$(echo "$ISP" | tr -c '[:alnum:]' '_')

# Function to run iperf3 test and log results
run_iperf3_test() {
    local server_name=$1
    local server_address=$2
    local IPERF_TEST_RESULT=$3
    
    # Log the start of the test
    echo "[$CURRENT_DATETIME] Starting iperf3 test to $server_name ($server_address)..." >> "$LOG_FILE"
    
    # Build iperf3 command with proper parameters for consistent parsing
    IPERF_CMD="iperf3 -c $server_address -p $PORT -t $DURATION -i $INTERVAL"
    
    # Run iperf3 and output only to file (not terminal)
    echo "----------------------------------------" >> "$LOG_FILE"
    echo "Test results for $server_name:" >> "$LOG_FILE"
    
    # Execute the command and save output to file only
    eval "$IPERF_CMD" > "$IPERF_TEST_RESULT" 2>> "$LOG_FILE"
    TEST_RESULT=$?
    
    echo "----------------------------------------" >> "$LOG_FILE"

    if [ $TEST_RESULT -eq 0 ]; then
        echo "[$CURRENT_DATETIME] Test completed successfully for $server_name." >> "$LOG_FILE"
        echo "Results saved to: $IPERF_TEST_RESULT" >> "$LOG_FILE"
    else
        echo "[$CURRENT_DATETIME] Error: Test failed for $server_name." >> "$LOG_FILE"
    fi
    echo "" >> "$LOG_FILE"  # Add an empty line for better readability
}

# Loop through each server and run the test
for SERVER_ADDRESS in "${SERVERS[@]}"; do
    # Extract server name from address (everything before first period)
    server_name="${SERVER_ADDRESS%%.*}"
    
    # Create consistent filename format that R script can parse correctly
    IPERF_TEST_RESULT="${RESULTS_FOLDER}/${USERNAME}_${ISP_SAFE}_${server_name}_iperf_test_${CURRENT_DATE}_${CURRENT_TIME}.csv"

    # Check for connectivity by pinging the server (silently)
    echo "[$CURRENT_DATETIME] Checking connectivity to $server_name..." >> "$LOG_FILE"
    ping -c 1 -q "$SERVER_ADDRESS" > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        run_iperf3_test "$server_name" "$SERVER_ADDRESS" "$IPERF_TEST_RESULT"
    else
        echo "[$CURRENT_DATETIME] Server $server_name unreachable. Aborting Test." >> "$LOG_FILE"
        echo "" >> "$LOG_FILE"
    fi
done

echo "[$CURRENT_DATETIME] All tests completed. Results stored in $RESULTS_FOLDER" >> "$LOG_FILE"