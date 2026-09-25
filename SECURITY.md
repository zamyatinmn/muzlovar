# Security policy

## Supported version

Security fixes are made on the current default branch. Older tags are not maintained as separate support lines.

## Reporting a vulnerability

Please do not disclose a suspected vulnerability in a public issue. Use GitHub's **Security → Report a vulnerability** flow to open a private security advisory for this repository.

Include the affected version or commit, deployment assumptions, reproduction steps, impact, and any suggested mitigation. Do not include real credentials or private user data.

## Deployment notes

Muzlovar starts without authentication when both `MUZLOVAR_USERNAME` and `MUZLOVAR_PASSWORD` are empty. Do not expose that configuration to an untrusted network. When authentication is enabled, terminate TLS at a trusted reverse proxy; HTTP Basic Auth does not encrypt traffic by itself.

The service writes playlist files and may delete or replace files it previously published. Give the container access only to the intended rules, playlists, and trash directories. Keep the process non-root and do not mount broader media, home, or system directories.

Subsonic credentials grant access to the configured Navidrome server. Store them outside version control, restrict their permissions, and rotate them if they are ever committed or logged.
