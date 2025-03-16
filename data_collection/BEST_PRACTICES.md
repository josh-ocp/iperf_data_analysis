# Best Practices for iPerf3 Data Collection

To ensure consistent and high-quality measurements, follow these best practices when collecting network performance data.

## Test Setup

### 1. Server Selection
- Use geographical-diverse servers for comprehensive testing
- Ensure servers are not overloaded
- Test to both nearby and distant servers

### 2. Test Duration
- Use at least 60 seconds for reliable averages (120s recommended)
- For stability analysis, longer tests (5+ minutes) are better
- For quick checks, 30 seconds is the minimum recommended duration

### 3. Testing Environment
- Minimize other network activity during tests
- Note any potential interference sources
- Run tests at different times of day
- If testing Wi-Fi, note your distance from access point

## Command-line Options

### Recommended Options
- `-i 1`: Report statistics every 1 second (good granularity)
- `-t 120`: Run for 120 seconds
- `-P 3`: Use 3 parallel connections to better utilize bandwidth

### Script Options

Our `run_iperf_test.sh` script provides simplified access to these options:
- `-s SERVER`: Server to test (can specify multiple times)
- `-d SECONDS`: Test duration in seconds (default: 120)
- `-u USER`: Username to record (default: current user)

### Example Commands

Basic test:
```bash
./run_iperf_test.sh -s speedtest.serverius.net -d 120
```

Advanced test:
```bash
./run_iperf_test.sh -s speedtest.serverius.net -d 300
```

Multiple servers:
```bash
./run_iperf_test.sh -s speedtest.serverius.net -s speedtest.london.linode.com -d 120
```

## Metadata and Naming

The script automatically collects and formats:
- Username
- ISP name
- Server name
- Timestamp

This ensures proper tracking and categorization in the analysis.

## Common Issues

1. **Connectivity failures**: The script attempts to ping servers first to avoid hanging.
2. **Disk space**: Long-term testing may accumulate large data files. Monitor available space.
