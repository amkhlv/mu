# mu

## YAML Reader Sample Program

This repository contains a sample Haskell program that reads a YAML file and prints the resulting object to stdout in JSON format.

The program uses `optparse-applicative` for command-line argument parsing, demonstrating basic CLI functionality.

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal (Haskell build tool)

### Building

```bash
cabal build --allow-newer
```

### Usage

```bash
cabal run --allow-newer yaml-reader FILE [OPTIONS]
```

#### Available Options

- `FILE` - YAML file to read and convert to JSON (required)
- `-c, --compact` - Output compact JSON instead of pretty-printed
- `-v, --version` - Show version information
- `-h, --help` - Show help text

#### Examples

Basic usage (pretty-printed JSON):
```bash
cabal run --allow-newer yaml-reader example.yaml
```

Compact JSON output:
```bash
cabal run --allow-newer yaml-reader --compact example.yaml
```

Show version:
```bash
cabal run --allow-newer yaml-reader --version
```

Show help:
```bash
cabal run --allow-newer yaml-reader --help
```

### Example Output

Given an input YAML file like `example.yaml`:
```yaml
name: John Doe
age: 30
email: john.doe@example.com
address:
  street: 123 Main St
  city: Springfield
  state: IL
  zip: "62701"
hobbies:
  - reading
  - hiking
  - programming
active: true
balance: 1234.56
```

The program outputs (by default, pretty-printed):
```json
{
    "active": true,
    "address": {
        "city": "Springfield",
        "state": "IL",
        "street": "123 Main St",
        "zip": "62701"
    },
    "age": 30,
    "balance": 1234.56,
    "email": "john.doe@example.com",
    "hobbies": [
        "reading",
        "hiking",
        "programming"
    ],
    "name": "John Doe"
}
```

With the `--compact` flag, the output is a single line:
```json
{"active":true,"address":{"city":"Springfield","state":"IL","street":"123 Main St","zip":"62701"},"age":30,"balance":1234.56,"email":"john.doe@example.com","hobbies":["reading","hiking","programming"],"name":"John Doe"}
```

### Implementation Details

The program uses the following Haskell libraries:
- `yaml` - For parsing YAML files
- `aeson` - For JSON representation
- `aeson-pretty` - For pretty-printing JSON output
- `optparse-applicative` - For command-line argument parsing

The program reads a YAML file, parses it into an Aeson Value, and prints it as formatted JSON to stdout.