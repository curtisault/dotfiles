# Cron Jobs for Linux

## Active Cron Jobs

```bash
# Update lightpanda nightly build (10 AM and 10 PM daily)
0 10,22 * * * /home/curtisault/dotfiles/linux/cronjobs/update-lightpanda

# Lynis security audit (Sunday 3 AM)
0 3 * * 0 sudo lynis audit system --quick >> ~/logs/lynis.log 2>&1

# Clean trash items older than 30 days (1st of month, 4 AM)
0 4 1 * * find ~/.local/share/Trash -mtime +30 -delete 2>/dev/null

# Clean stale /tmp files older than 7 days (Sunday 4:30 AM)
30 4 * * 0 find /tmp -type f -atime +7 -delete 2>/dev/null

# Flush DNS cache (every 30 minutes)
*/30 * * * * resolvectl flush-caches

# Remove all cached versions of uninstalled packages (Sunday 3:30 AM)
30 3 * * 0 sudo paccache -ruk0
```

## System Maintenance

```bash
# Clean package manager cache weekly (Sunday at 3 AM)
# Arch: paccache -r | Debian/Ubuntu: sudo apt clean
0 3 * * 0 paccache -r

# Clean orphaned packages weekly (Sunday at 4 AM)
# Arch: pacman -Qdtq | sudo pacman -Rns - | Debian/Ubuntu: sudo apt autoremove
0 4 * * 0 pacman -Qdtq | sudo pacman -Rns --noconfirm - 2>/dev/null

# Clean old journal logs monthly (1st of month, 3 AM)
0 3 1 * * sudo journalctl --vacuum-time=30d

# Clean Downloads folder (files older than 30 days, weekly)
0 5 * * 0 find ~/Downloads -type f -mtime +30 -delete

# Clear user caches monthly (1st of month, 4 AM)
0 4 1 * * rm -rf ~/.cache/*
```

## Backups

```bash
# Backup important directories daily (1 AM)
0 1 * * * rsync -av ~/Documents/ /mnt/backup/Documents/

# Database dumps (example for postgres)
0 2 * * * pg_dump mydb > ~/backups/mydb_$(date +\%Y\%m\%d).sql

# Backup SSH keys and config weekly (Sunday 2 AM)
0 2 * * 0 tar -czf ~/backups/ssh_backup_$(date +\%Y\%m\%d).tar.gz ~/.ssh

# Clean old backups (keep last 30 days, weekly)
0 5 * * 0 find ~/backups -name "*.sql" -mtime +30 -delete
0 5 * * 0 find ~/backups -name "*.tar.gz" -mtime +30 -delete
```

## Development

```bash
# Clean Docker resources weekly (Saturday 3 AM)
0 3 * * 6 docker system prune -af

# Git fetch all repos in projects folder (daily)
0 5 * * * find ~/projects -name .git -type d -execdir git fetch --all \;

# Clean old node_modules in projects (older than 90 days, weekly)
0 6 * * 0 find ~/projects -name "node_modules" -type d -mtime +90 -exec rm -rf {} +
```

## Monitoring & Logs

```bash
# Disk space alert (daily at 9 AM)
0 9 * * * df -h | grep -E '^/dev/' | awk '{if($5+0 > 80) print}'

# Check internet connectivity and log downtime
*/5 * * * * ping -c 1 8.8.8.8 > /dev/null 2>&1 || echo "$(date): Internet down" >> ~/logs/connectivity.log

# Rotate large log files (weekly)
0 7 * * 0 find ~/logs -name "*.log" -size +100M -exec gzip {} \;
```

## Productivity

```bash
# Daily reminder to commit work (5 PM weekdays)
0 17 * * 1-5 DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus" notify-send "End of Day" "Commit your work!"

# Break reminder every 2 hours during work hours (weekdays)
0 10,12,14,16 * * 1-5 DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus" notify-send "Health Reminder" "Take a break!"
```

## Setup

The active cron jobs are backed up to `cachy_os/crontab` and can be installed via the CachyOS setup script at `linux/bin/cachy_os/setup`, which also configures the required sudoers entries for `lynis` and `paccache`.

## Important Notes

### Prerequisites

Install and enable `cronie`:

```bash
# Arch: sudo pacman -S cronie
# Debian/Ubuntu: sudo apt install cron
sudo systemctl enable --now cronie
```

### Edit crontab

```bash
crontab -e  # Edit your crontab
crontab -l  # List current cron jobs
crontab -r  # Remove all cron jobs
```

### Cron time format

```
* * * * * command
| | | | |
| | | | +- Day of week (0-7, Sunday = 0 or 7)
| | | +--- Month (1-12)
| | +----- Day of month (1-31)
| +------- Hour (0-23)
+--------- Minute (0-59)
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

### Desktop notifications from cron

Cron jobs don't have access to the D-Bus session. Set the bus address before calling `notify-send`:

```bash
DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus" notify-send "Title" "Message"
```

### Redirecting output

```bash
# Redirect output to log file
0 2 * * * /path/to/script.sh >> ~/logs/script.log 2>&1

# Send errors to different file
0 2 * * * /path/to/script.sh >> ~/logs/script.log 2>> ~/logs/script_errors.log

# Suppress all output
0 2 * * * /path/to/script.sh > /dev/null 2>&1
```

### Environment variables

Cron runs with a minimal environment. If your scripts need specific paths or variables:

```bash
# At the top of your crontab
SHELL=/bin/bash
PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

# Or source your profile in individual jobs
0 2 * * * . ~/.bash_profile; /path/to/script.sh
```
