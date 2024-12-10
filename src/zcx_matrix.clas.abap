CLASS zcx_matrix DEFINITION INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      c_err_invalid_row_col     TYPE string VALUE 'Invalid row or column index: (Row: &1, Col: &2).',
      c_err_matrix_size_empty   TYPE string VALUE 'Matrix size must be greater than zero.',
      c_err_inconsistent_matrix TYPE string VALUE 'Inconsistent matrix supplied',
      c_err_unequal_matrix      TYPE string VALUE 'Matrix dimensions do not match.',
      c_err_division_by_zero    TYPE string VALUE 'Division by zero is not allowed.'.
    METHODS constructor
      IMPORTING iv_message    TYPE clike
                iv_message_v1 TYPE clike OPTIONAL
                iv_message_v2 TYPE clike OPTIONAL
                iv_message_v3 TYPE clike OPTIONAL
                iv_message_v4 TYPE clike OPTIONAL.

    CLASS-METHODS raise
      IMPORTING iv_message    TYPE clike
                iv_message_v1 TYPE any OPTIONAL
                iv_message_v2 TYPE any OPTIONAL
                iv_message_v3 TYPE any OPTIONAL
                iv_message_v4 TYPE any OPTIONAL
      RAISING   zcx_matrix.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:message TYPE string.
ENDCLASS.



CLASS zcx_matrix IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).

    message = iv_message.

    REPLACE ALL OCCURRENCES OF '&1' IN me->message WITH iv_message_v1.
    REPLACE ALL OCCURRENCES OF '&2' IN me->message WITH iv_message_v2.
    REPLACE ALL OCCURRENCES OF '&3' IN me->message WITH iv_message_v3.
    REPLACE ALL OCCURRENCES OF '&4' IN me->message WITH iv_message_v4.
  ENDMETHOD.

  METHOD raise.
    RAISE EXCEPTION TYPE zcx_matrix
      EXPORTING
        iv_message    = iv_message
        iv_message_v1 = CONV string( iv_message_v1 )
        iv_message_v2 = CONV string( iv_message_v2 )
        iv_message_v3 = CONV string( iv_message_v3 )
        iv_message_v4 = CONV string( iv_message_v4 ).
  ENDMETHOD.
ENDCLASS.
