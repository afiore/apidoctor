# 👩‍⚕️ APIDoctor

 A cli tool to sanity check OpenAPI specs

## Usage

```
apidoctor-lint 0.1.0

USAGE:
    apidoctor lint [FLAGS] [OPTIONS] <spec>

FLAGS:
    -h, --help       Prints help information
    -s, --summary    Display only linting counts, omitting detailed messages
    -V, --version    Prints version information
    -w, --watch      Watch spec file for changes

OPTIONS:
    -o, --operation-id <operation-id>    Filter issue by the given operation id
    -t, --tags <tags>...                 Filter issue by the given set of tags

ARGS:
    <spec>    path to JSON or YAML spec
```

### Available linters

Currently the following linters have been implemented, and are always executed.

- example request/response payloads that fail schema validation.
- operations that have have a request/response schema, but no example payloads.
- presence of key metadata: description/summary and tags

### ⚠️ Warning

This project is at an early development stage, and might not be actively maintained in the future.
