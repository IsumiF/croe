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
    "numConns": 10
  }
}
```

### Build Tools
- `nix`
- `stack`
- `sass`
- `google-closure-compiler`
- `mysql_config`
