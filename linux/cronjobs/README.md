# Useful Cron Jobs for MacBook

## System Maintenance

```bash
# Clean up Homebrew caches weekly (Sunday at 3 AM)
0 3 * * 0 brew cleanup --prune=all

# Empty trash weekly (Sunday at 4 AM)
0 4 * * 0 rm -rf ~/.Trash/*

# Clear DNS cache daily (2 AM)
0 2 * * * sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder

# Update locate database daily (3 AM)
0 3 * * * sudo /usr/libexec/locate.updatedb

# Clean Downloads folder (files older than 30 days, weekly)
0 5 * * 0 find ~/Downloads -type f -mtime +30 -delete

# Clean old screenshots from Desktop (older than 7 days, daily)
0 6 * * * find ~/Desktop -name "Screen Shot*.png" -mtime +7 -delete

# Clear system caches monthly (1st of month, 3 AM)
0 3 1 * * sudo rm -rf /Library/Caches/*

# Clear user caches monthly (1st of month, 4 AM)
0 4 1 * * rm -rf ~/Library/Caches/*
```

## Backups

```bash
# Backup important directories daily (1 AM)
0 1 * * * rsync -av ~/Documents/ /Volumes/Backup/Documents/

# Time Machine backup trigger (if not using auto)
0 */4 * * * tmutil startbackup

# Database dumps (example for postgres)
0 2 * * * pg_dump mydb > ~/backups/mydb_$(date +\%Y\%m\%d).sql

# Backup SSH keys and config weekly (Sunday 2 AM)
0 2 * * 0 tar -czf ~/backups/ssh_backup_$(date +\%Y\%m\%d).tar.gz ~/.ssh

# Backup dotfiles weekly (Sunday 2:30 AM)
30 2 * * 0 tar -czf ~/backups/dotfiles_$(date +\%Y\%m\%d).tar.gz ~/dotfiles

# Clean old backups (keep last 30 days, weekly)
0 5 * * 0 find ~/backups -name "*.sql" -mtime +30 -delete
0 5 * * 0 find ~/backups -name "*.tar.gz" -mtime +30 -delete
```

## Development

```bash
# Update package managers daily (4 AM)
0 4 * * * brew update && brew upgrade

# Clean Docker resources weekly (Saturday 3 AM)
0 3 * * 6 docker system prune -af

# Git fetch all repos in projects folder (daily)
0 5 * * * find ~/projects -name .git -type d -execdir git fetch --all \;

# Update global npm packages weekly (Sunday 4 AM)
0 4 * * 0 npm update -g

# Clean old node_modules in projects (older than 90 days, weekly)
0 6 * * 0 find ~/projects -name "node_modules" -type d -mtime +90 -exec rm -rf {} +

# Update Elixir/Hex weekly (Sunday 5 AM)
0 5 * * 0 mix local.hex --force && mix local.rebar --force

# Clean Elixir build artifacts older than 30 days (weekly)
0 6 * * 0 find ~/projects -name "_build" -type d -mtime +30 -exec rm -rf {} +

# Update asdf plugins monthly (1st of month, 5 AM)
0 5 1 * * asdf plugin update --all
```

## Monitoring & Logs

```bash
# Disk space alert (daily at 9 AM)
0 9 * * * df -h | grep -E '^/dev/' | awk '{if($5+0 > 80) print}'

# Clean old log files (weekly)
0 3 * * 0 find ~/Library/Logs -name "*.log" -mtime +30 -delete

# Monitor system temperature/performance
*/30 * * * * powermetrics --samplers smc -i1 -n1 >> ~/logs/temp.log

# Check internet connectivity and log downtime
*/5 * * * * ping -c 1 8.8.8.8 > /dev/null 2>&1 || echo "$(date): Internet down" >> ~/logs/connectivity.log

# Monitor battery health weekly (Sunday 10 AM)
0 10 * * 0 system_profiler SPPowerDataType >> ~/logs/battery_$(date +\%Y\%m\%d).log

# Rotate large log files (weekly)
0 7 * * 0 find ~/logs -name "*.log" -size +100M -exec gzip {} \;
```

## Productivity

```bash
# Daily reminder to commit work (5 PM weekdays)
0 17 * * 1-5 osascript -e 'display notification "Commit your work!" with title "End of Day"'

# Weekly project review reminder (Friday 4 PM)
0 16 * * 5 osascript -e 'display notification "Weekly review time!" with title "Friday Review"'

# Morning standup reminder (9:30 AM weekdays)
30 9 * * 1-5 osascript -e 'display notification "Time for standup!" with title "Daily Standup"'

# Break reminder every 2 hours during work hours (weekdays)
0 10,12,14,16 * * 1-5 osascript -e 'display notification "Take a break!" with title "Health Reminder"'
```

## Security & Updates

```bash
# Check for macOS updates weekly (Monday 9 AM)
0 9 * * 1 softwareupdate -l >> ~/logs/updates.log

# Check for security updates daily (6 AM)
0 6 * * * softwareupdate -l --recommended >> ~/logs/security_updates.log

# Verify SSH key permissions daily
0 7 * * * chmod 700 ~/.ssh && chmod 600 ~/.ssh/*

# Clean browser caches weekly (Sunday 5 AM)
0 5 * * 0 rm -rf ~/Library/Caches/Google/Chrome/*
0 5 * * 0 rm -rf ~/Library/Caches/Firefox/*
```

## Important Notes

### macOS prefers `launchd` over cron

For more reliable scheduling, especially for tasks requiring GUI access or running when logged out, consider using `launchd` plist files in `~/Library/LaunchAgents/`.

### Edit crontab

```bash
crontab -e  # Edit your crontab
crontab -l  # List current cron jobs
crontab -r  # Remove all cron jobs
```

### Cron time format

```
* * * * * command
│ │ │ │ │
│ │ │ │ └─ Day of week (0-7, Sunday = 0 or 7)
│ │ │ └─── Month (1-12)
│ │ └───── Day of month (1-31)
│ └─────── Hour (0-23)
└───────── Minute (0-59)
```

### Special time strings

```bash
@reboot     # Run at startup
@daily      # Run once a day (0 0 * * *)
@weekly     # Run once a week (0 0 * * 0)
@monthly    # Run once a month (0 0 1 * *)
@yearly     # Run once a year (0 0 1 1 *)
@hourly     # Run once an hour (0 * * * *)
```

### Permissions

macOS may require granting cron **Full Disk Access** in:
- System Settings → Privacy & Security → Full Disk Access

Add `/usr/sbin/cron` to the list of allowed applications.

### Redirecting Output

```bash
# Redirect output to log file
0 2 * * * /path/to/script.sh >> ~/logs/script.log 2>&1

# Send errors to different file
0 2 * * * /path/to/script.sh >> ~/logs/script.log 2>> ~/logs/script_errors.log

# Suppress all output
0 2 * * * /path/to/script.sh > /dev/null 2>&1
```

### Environment Variables

Cron runs with a minimal environment. If your scripts need specific paths or variables:

```bash
# At the top of your crontab
SHELL=/bin/bash
PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
MAILTO=your@email.com

# Or in individual jobs
0 2 * * * . ~/.bash_profile; /path/to/script.sh
```
