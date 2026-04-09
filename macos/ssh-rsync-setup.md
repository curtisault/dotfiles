# SSH Key Authentication & Rsync Setup

A guide for connecting two machines on a local network using SSH key authentication and transferring files with rsync.

---

## 1. Prerequisites

- Both machines on the same local network
- **Remote Login enabled** on the source machine (the one you are pulling files *from*):
  - **System Settings → General → Sharing → Remote Login → On**
  - Set to "All Users" or add your user explicitly
- Your macOS account must have a password set (blank passwords are rejected by sshd)

---

## 2. Find the Machine on Your Network

### Get the source machine's Bonjour name

Run on the **source machine**:

```sh
scutil --get LocalHostName
```

This returns the machine's mDNS hostname — for example, `MyMacBook`. You can then reach it on the local network as `MyMacBook.local`. This guide uses `MyMacBook` as a placeholder; substitute your actual hostname throughout.

### Get the username

Run on whichever machine you need the username for:

```sh
whoami
```

Use this output in place of `user` throughout the commands in this guide (e.g. `ssh-copy-id user@MyMacBook.local`). The same key pair can authenticate to multiple services (GitHub, remote machines, etc.) — copying it to a new machine does not affect existing connections.

### Get the source machine's IP address

```sh
ipconfig getifaddr en0   # Wi-Fi
ipconfig getifaddr en1   # Ethernet
```

### Discover devices on the network from the other machine

```sh
# mDNS — find machines advertising SSH
dns-sd -B _ssh._tcp .

# Ping by Bonjour name
ping MyMacBook.local

# Scan entire subnet (requires nmap)
nmap -sn 192.168.1.0/24

# Check ARP cache for recently seen devices
arp -a
```

---

## 3. Generate an SSH Key Pair

Run on the **client machine** (the one you are connecting *from*):

```sh
ssh-keygen -t ed25519
```

- Press Enter to accept the default path (`~/.ssh/id_ed25519`)
- Optionally set a passphrase for extra security (you will enter it when connecting)

This creates two files:

| File | Description |
|------|-------------|
| `~/.ssh/id_ed25519` | Private key — never share this |
| `~/.ssh/id_ed25519.pub` | Public key — copied to the server |

---

## 4. Copy the Public Key to the Source Machine

### Option A — ssh-copy-id (easiest)

```sh
ssh-copy-id user@MyMacBook.local
```

This will prompt for your password once, then install the public key automatically.

### Option B — Manual

If `ssh-copy-id` is unavailable or fails:

**Step 1:** Print your public key on the client machine:

```sh
cat ~/.ssh/id_ed25519.pub
```

**Step 2:** Copy the entire output. Then on the **source machine**, run:

```sh
mkdir -p ~/.ssh
echo "paste-your-public-key-here" >> ~/.ssh/authorized_keys
```

---

## 5. Set Correct Permissions on the Source Machine

SSH is strict about file permissions. If the `.ssh` directory or `authorized_keys` file is accessible by other users, `sshd` will refuse to use them — it assumes they may have been tampered with.

```sh
chmod 700 ~/.ssh
chmod 600 ~/.ssh/authorized_keys
```

### What these permissions mean

| Permission | Octal | Meaning |
|------------|-------|---------|
| `chmod 700 ~/.ssh` | `rwx------` | Only your user can read, write, and enter the directory. Group and others have no access. |
| `chmod 600 ~/.ssh/authorized_keys` | `rw-------` | Only your user can read and write the file. Group and others have no access. |

Verify with:

```sh
ls -la ~/.ssh
```

Expected output:

```
drwx------  user  .ssh/
-rw-------  user  authorized_keys
```

---

## 6. Test the SSH Connection

### Basic connection test

From the **client machine**:

```sh
ssh user@MyMacBook.local
# or by IP:
ssh user@192.168.1.42
```

### Force password authentication (bypass key issues)

```sh
ssh -o PreferredAuthentications=password user@MyMacBook.local
```

### Verbose output for debugging

```sh
ssh -v user@MyMacBook.local
```

Look for lines near the bottom mentioning `publickey` or `password` — they show exactly where authentication is succeeding or failing.

### Check sshd logs on the source machine

```sh
log show --last 5m --predicate 'process == "sshd"' | tail -30
```

This shows the exact rejection reason (e.g. `Failed password`, `User not allowed`, `bad permissions`).

---

## 7. Check sshd Configuration (if still failing)

On the **source machine**:

```sh
grep -E "PasswordAuth|PermitRoot|AllowUsers" /etc/ssh/sshd_config
```

If `PasswordAuthentication no` is set, password logins are disabled. You can change it to `yes` and restart SSH:

```sh
sudo launchctl stop com.openssh.sshd
sudo launchctl start com.openssh.sshd
```

---

## 8. Transfer Files with Rsync

Once SSH is working, use rsync to copy files. Rsync uses SSH under the hood.

### Basic syntax

```sh
rsync -avz source/ destination/
```

### Common flags

| Flag | Description |
|------|-------------|
| `-a` | Archive mode — preserves permissions, timestamps, symlinks, owner, group |
| `-v` | Verbose — shows files being transferred |
| `-z` | Compress data during transfer |
| `-n` / `--dry-run` | Preview what would be transferred without copying |
| `--progress` | Show per-file transfer progress |
| `--delete` | Delete files on destination that no longer exist on source |
| `--exclude='pattern'` | Exclude files matching a pattern |

### Pull files FROM the source machine (run on client)

```sh
rsync -avz user@MyMacBook.local:/path/to/files/ /local/destination/
```

### Push files TO the source machine (run on client)

```sh
rsync -avz /local/files/ user@MyMacBook.local:/remote/destination/
```

### Dry run first (recommended)

```sh
rsync -avzn user@MyMacBook.local:~/Documents/ ~/Documents/
```

### Custom SSH port

```sh
rsync -avz -e "ssh -p 2222" user@MyMacBook.local:/src/ /dest/
```

### Exclude macOS junk files

```sh
rsync -avz --exclude='.DS_Store' --exclude='__MACOSX' user@MyMacBook.local:/src/ /dest/
```

---

## 9. Source Path Trailing Slash — Important

The trailing slash on the **source** path changes behavior:

```sh
# Copies the CONTENTS of Documents/ into destination/
rsync -avz ~/Documents/ user@MyMacBook.local:/destination/

# Copies the Documents DIRECTORY ITSELF into destination/
rsync -avz ~/Documents user@MyMacBook.local:/destination/
# Result: /destination/Documents/
```

When in doubt, use `--dry-run` first to confirm what will be transferred.

---

## Summary

1. Enable Remote Login on the source machine
2. Ensure your account has a password set (`passwd`)
3. Find the source machine's hostname: `scutil --get LocalHostName` (e.g. `MyMacBook`)
4. Generate an SSH key pair on the client (`ssh-keygen -t ed25519`)
5. Copy the public key to the source machine (`ssh-copy-id user@MyMacBook.local` or manually)
6. Set correct permissions on `~/.ssh` (700) and `authorized_keys` (600)
7. Test the connection (`ssh user@MyMacBook.local`)
8. Use rsync to transfer files (`rsync -avz user@MyMacBook.local:/src/ /dest/`)
