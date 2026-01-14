# mu

## YAML Reader Sample Program

This repository contains a sample Haskell program that reads a YAML file and prints the resulting object to stdout in JSON format.

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal (Haskell build tool)

### Building

```bash
cabal build --allow-newer
```

### Usage

```bash
cabal run --allow-newer yaml-reader <filename.yaml>
```

Example:
```bash
cabal run --allow-newer yaml-reader example.yaml
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

The program outputs:
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

### Implementation Details

The program uses the following Haskell libraries:
- `yaml` - For parsing YAML files
- `aeson` - For JSON representation
- `aeson-pretty` - For pretty-printing JSON output

The program reads a YAML file, parses it into an Aeson Value, and prints it as formatted JSON to stdout.