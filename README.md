# APIDoctor

A cli tool to sanity check OpenAPI specs

## Usage

```
apidoctor 0.1.0
An API spec linter

USAGE:
    apidoctor [OPTIONS] <spec>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -o, --operation-id <operation-id>    Filter issue by the given operation id
    -t, --tags <tags>...                 Filter issue by the given set of tags

ARGS:
    <spec>    path to JSON or YAML spec
```

### Available linters

Currently the following linters have been implemented:

- example request/response payloads that fail schema validation
- operations that have have a request/response schema, but no example payloads

### ⚠️ Warning

This project is at an early development stage, and might not be actively maintained in the future.