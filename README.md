# croe

![CI](https://github.com/IsumiF/croe/workflows/CI/badge.svg)

A task system built with fullstack haskell tech

## Setup

### Config File

Example config:
```
{
  "logger": {
    "level": "Debug"
  },
  "persist": {
    "host": "127.0.0.1",
    "port": 3306,
    "user": "root",
    "password": "dev",
    "database": "croe",
    "numConns": 10,
    "migrate": true
  },
  "mail": {
    "smtpHost": "smtp.gmail.com",
    "username": "your email address",
    "password": "your password"
  },
  "authService": {
    "cacheExpiration": 30
  }
}
```

### Build Tools
- `nix`
- `stack`
- `sass`
- `google-closure-compiler`
- `mysql_config`

## Develop

### Modify test config

1. edit file `backend/config/test.json`
2. encrypt with `gpg --symmetric --cipher-algo AES256 test.json`
