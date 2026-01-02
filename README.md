# Matrix and Vector DSL

A domain-specific language (DSL) for matrix and vector operations, implemented in OCaml. This language provides comprehensive support for mathematical operations on scalars, vectors, and matrices with static type checking and a custom interpreter.

## Features

### Supported Data Types
- **Integers**: Basic integer values
- **Floats**: Floating-point numbers
- **Booleans**: True/false values
- **Vectors**: One-dimensional arrays with fixed size
- **Matrices**: Two-dimensional arrays with fixed dimensions
- **Files**: File I/O support

### Operations

#### Matrix and Vector Operations
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `^` (power)
- **Matrix Operations**: 
  - Transpose (`^`)
  - Determinant (`Det`)
  - Matrix multiplication (`**`)
  - Dot product (`.`)
  - Minor extraction
  - Row operations (swap, reduce)
- **Vector Operations**:
  - Magnitude (`Mag`)
  - Dimension (`Dim`)
  - Angle between vectors
- **Utility Operations**:
  - Size (`Size`)
  - Rows (`Rows`)
  - Cols (`Cols`)
  - Absolute value (`Abs`)
  - Zero checking (`Iszero`)

#### Control Flow
- **Conditional**: `if-then-else` statements
- **Loops**: `for` and `while` loops
- **Blocks**: Sequential statement execution with scoping

#### I/O Operations
- **Print**: Display values to console
- **Input**: Read from user input or files
- **File Reading**: Load matrices and vectors from files

### Type System
The language features a **static type checker** that ensures:
- Type safety for all operations
- Dimension compatibility for matrix/vector operations
- Proper variable declarations and usage



## Installation

### Prerequisites
- OCaml compiler (`ocamlc`)
- OCamllex (lexer generator)
- OCamlyacc (parser generator)

### Build

Compile the project using the provided Makefile:

```bash
make
```

This will generate the `my_program` executable.

### Clean

Remove generated files:

```bash
make clean
```

## Usage

### Running a Program

Execute a DSL program from a file:

```bash
./my_program <filename.dsl>
```

Or run interactively (stdin):

```bash
./my_program
```

### Example Programs

#### Basic Matrix Operations
```dsl
{
  A := {[1,2,3],[4,5,6]};
  B := {[1,1,1],[1,1,1]};
  C := A + B;
  D := A[0];
  Print(C);
  Print(D);
}
```

#### Matrix Transpose
```dsl
{
  A := {[1,2,3],[4,5,6]};
  D := A^;
  Print(D);
}
```

#### Matrix Element Access and Assignment
```dsl
{
  E := {[1,2],[3,4]};
  F := E[1][1];
  E[1][1] := 12;
  Print(F);
  Print(E);
}
```

## Language Syntax

### Variable Assignment
```dsl
variable := expression;
```

### Matrix/Vector Literals
```dsl
vector := {[1, 2, 3]};
matrix := {[1, 2], [3, 4]};
```

### Indexing
```dsl
vector[index]          (* Access vector element *)
matrix[row][col]       (* Access matrix element *)
```

### Comments
```dsl
(* This is a comment *)
```

## VS Code Extension

The project includes a VS Code extension (`greninja/`) for syntax highlighting of `.dsl` files.

### Installation
1. Navigate to the `greninja/` directory
2. Install the extension in VS Code
3. Enjoy syntax highlighting for your DSL programs!

## Error Handling

The interpreter provides comprehensive error messages for:
- **Lexical errors**: Invalid tokens or characters
- **Syntax errors**: Grammar violations
- **Type errors**: Type mismatches and incompatible operations
- **Runtime errors**: Division by zero, index out of bounds, etc.

## Test Cases

Sample test cases are available in:
- `A3_testcases/`: Assignment test cases
- `SamplePrograms/`: Example programs demonstrating various features

## Development

### Phases
1. **Lexical Analysis**: Tokenizes input using `lexer.mll`
2. **Parsing**: Builds AST using `parser.mly`
3. **Type Checking**: Validates types using `type_checker.ml`
4. **Interpretation**: Executes program using `interpreter.ml`

### Adding New Features
1. Update `ast.ml` with new AST nodes
2. Extend `lexer.mll` for new tokens
3. Update `parser.mly` with new grammar rules
4. Implement type checking in `type_checker.ml`
5. Add evaluation logic in `interpreter.ml`

## License

This project is part of COL 226 coursework at IIT.

## Authors

Developed as part of the Programming Languages course assignment.
