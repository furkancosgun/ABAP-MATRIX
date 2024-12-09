# ABAP Matrix Library

The **ABAP Matrix Package** is a comprehensive library designed to handle matrix operations in ABAP. It offers a wide range of functionalities, including basic arithmetic operations (addition, subtraction, multiplication, division), structural manipulations (reshaping, transposing, swapping rows/columns), and property checks (e.g., checking if a matrix is square, diagonal, or empty). This package is particularly useful for applications that involve matrix calculations and manipulations in ABAP, making it a powerful tool for mathematical and data processing tasks.


## Features

### Arithmetic Operations
- **Scalar Operations:** Add, subtract, multiply, or divide a matrix by a scalar value.
- **Matrix Operations:** Element-wise addition, subtraction, multiplication, and division between two matrices.
- **Dot Product:** Compute the dot product between matrices or specific rows/columns.

### Structural Manipulations
- **Reshape:** Change the dimensions of a matrix without altering its data.
- **Transpose:** Convert rows into columns and vice versa.
- **Swap Rows/Columns:** Swap specific rows or columns.
- **Merge:** Combine two matrices into one.
- **Clear and Copy:** Clear a matrix or create an exact copy.

### Property Checks
- Verify if a matrix is:
  - Square
  - Rectangular
  - Row/Column Vector
  - Diagonal
  - Empty
  - Single Item

### Utility Methods
- Retrieve specific rows, columns, or elements.
- Generate pre-defined matrices (zeros, ones, diagonal).
- Check equality or similarity between matrices.

---

## Getting Started

### Installation
-  Install **ABAPGit** in your SAP system if it's not already installed.
-  Open the **ABAPGit** application in your system.
-  Clone the repository URL of this project into **ABAPGit**.
-  Pull the code into your system. **ABAPGit** will automatically create the necessary objects in your package.

---

## Usage

### Creating a Matrix
```abap
DATA lo_matrix TYPE REF TO zcl_matrix.

" Create a 3x3 matrix filled with zeros
lo_matrix = zcl_matrix=>zeros( iv_rows = 3
                               iv_cols = 3 ).

" Create a 2x2 diagonal matrix
lo_matrix = zcl_matrix=>diagonal( iv_rows = 2
                                  iv_cols = 2 ).
```

### Basic Arithmetic
```abap
DATA lo_matrix       TYPE REF TO zcl_matrix.
DATA lo_other_matrix TYPE REF TO zcl_matrix.

lo_matrix = zcl_matrix=>zeros( iv_rows = 3
                               iv_cols = 3 ).

" Add a scalar to all elements
lo_matrix = lo_matrix->add_by_scalar( iv_val = 5 ).

lo_other_matrix = zcl_matrix=>ones( iv_rows = 3
                                    iv_cols = 3 ).

" Add two matrices
lo_matrix = lo_matrix->add_matrix( io_matrix = lo_other_matrix ).
```

### Structural Manipulation
```abap
DATA lo_matrix TYPE REF TO zcl_matrix.

lo_matrix = zcl_matrix=>ones( iv_rows = 6
                              iv_cols = 3 ).

" Transpose the matrix
lo_matrix = lo_matrix->transpose_matrix( ).

" Reshape the matrix to 1x9
lo_matrix = lo_matrix->reshape( iv_rows = 1
                                iv_cols = 9 ).
```

### Property Checks
```abap
DATA lo_matrix TYPE REF TO zcl_matrix.

lo_matrix = zcl_matrix=>diagonal( iv_rows = 3
                                  iv_cols = 3 ).

IF lo_matrix->is_square( ).
  WRITE 'Matrix is square'.
ENDIF.

IF lo_matrix->is_diagonal( ).
  WRITE 'Matrix is diagonal'.
ENDIF.
```

---

## API Documentation

### **Math Methods**

| Method Name       | Description                               |
|--------------------|-------------------------------------------|
| **ADD**           | Adds a value to the specified cell.       |
| **SUB**           | Subtracts a value from the specified cell.|
| **MUL**           | Multiplies the specified cell by a value. |
| **DIV**           | Divides the specified cell by a value.    |
| **ADD_BY_SCALAR** | Adds a scalar value to all matrix cells.  |
| **SUB_BY_SCALAR** | Subtracts a scalar value from all cells.  |
| **MUL_BY_SCALAR** | Multiplies all matrix cells by a scalar.  |
| **DIV_BY_SCALAR** | Divides all matrix cells by a scalar.     |
| **ADD_MATRIX**    | Adds another matrix to the current one.   |
| **SUB_MATRIX**    | Subtracts another matrix from the current one.|
| **MUL_MATRIX**    | Multiplies the current matrix with another.|
| **DIV_MATRIX**    | Divides the current matrix by another one.|
| **DOT**           | Performs dot product with another matrix.|
| **DOT_ROW**       | Performs dot product for a specific row.  |
| **DOT_COL**       | Performs dot product for a specific column.|

---

### **Properties Methods**

| Method Name        | Description                                    |
|---------------------|------------------------------------------------|
| **IS_SQUARE**      | Checks if the matrix is square.                |
| **IS_RECTANGLE**   | Checks if the matrix is rectangular.           |
| **IS_ROW_VECTOR**  | Checks if the matrix is a row vector.          |
| **IS_COL_VECTOR**  | Checks if the matrix is a column vector.       |
| **IS_VECTOR**      | Checks if the matrix is a vector.              |
| **IS_DIAGONAL**    | Checks if the matrix is diagonal.              |
| **IS_EMPTY**       | Checks if the matrix is empty.                 |
| **IS_SINGLE_ITEM** | Checks if the matrix contains a single item.   |
| **SIZE**           | Returns the size of the matrix.                |

---

### **Structure Methods**

| Method Name          | Description                                     |
|-----------------------|-------------------------------------------------|
| **RESHAPE**          | Reshapes the matrix to specified dimensions.    |
| **SWAP_ROW**         | Swaps two rows in the matrix.                   |
| **SWAP_COL**         | Swaps two columns in the matrix.                |
| **TRANSPOSE_MATRIX** | Returns the transpose of the matrix.            |
| **MERGE**            | Merges the matrix with another matrix.          |
| **CLEAR**            | Clears all values in the matrix.                |
| **COPY**             | Creates a copy of the matrix.                   |
| **IS_SAME**          | Checks if another matrix has the same structure.|
| **IS_EQUALS**        | Checks if another matrix is equal in values.    |

---

### **Instance Methods**

| Method Name      | Description                                      |
|-------------------|--------------------------------------------------|
| **FULL**         | Creates a matrix filled with a specified value.  |
| **ONES**         | Creates a matrix filled with ones.               |
| **ZEROS**        | Creates a matrix filled with zeros.              |
| **DIAGONAL**     | Creates a diagonal matrix with given dimensions. |
| **FROM_TABLE**   | Creates a matrix from a given internal table.    |

---

## Contributing
Contributions are welcome! Please fork the repository and submit a pull request with a detailed explanation of your changes.

---

## License
This project is open-source and available under the [MIT](LICENSE) License.

---
