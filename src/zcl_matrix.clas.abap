CLASS zcl_matrix DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_matrix_value  TYPE i.

    TYPES ty_matrix_values TYPE STANDARD TABLE OF ty_matrix_value WITH EMPTY KEY.

    TYPES: BEGIN OF ty_matrix,
             row TYPE i,
             col TYPE i,
             val TYPE ty_matrix_value,
           END OF ty_matrix.

    TYPES ty_matrix_tab TYPE HASHED TABLE OF ty_matrix WITH UNIQUE KEY row col.

    " Math
    METHODS add
      IMPORTING iv_row TYPE i
                iv_col TYPE i
                iv_val TYPE ty_matrix_value.

    METHODS sub
      IMPORTING iv_row TYPE i
                iv_col TYPE i
                iv_val TYPE ty_matrix_value.

    METHODS mul
      IMPORTING iv_row TYPE i
                iv_col TYPE i
                iv_val TYPE ty_matrix_value.

    METHODS div
      IMPORTING iv_row TYPE i
                iv_col TYPE i
                iv_val TYPE ty_matrix_value.

    METHODS add_by_scalar
      IMPORTING iv_val           TYPE ty_matrix_value
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS sub_by_scalar
      IMPORTING iv_val           TYPE ty_matrix_value
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS mul_by_scalar
      IMPORTING iv_val           TYPE ty_matrix_value
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS div_by_scalar
      IMPORTING iv_val           TYPE ty_matrix_value
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS add_matrix
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS sub_matrix
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS mul_matrix
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS div_matrix
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS dot
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS dot_row
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
                iv_row           TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS dot_col
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
                iv_col           TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    " Props
    METHODS is_square
      RETURNING VALUE(rv_square) TYPE abap_bool.

    METHODS is_rectangle
      RETURNING VALUE(rv_rectangle) TYPE abap_bool.

    METHODS is_row_vector
      RETURNING VALUE(rv_vector) TYPE abap_bool.

    METHODS is_col_vector
      RETURNING VALUE(rv_vector) TYPE abap_bool.

    METHODS is_vector
      RETURNING VALUE(rv_vector) TYPE abap_bool.

    METHODS is_diagonal
      RETURNING VALUE(rv_diagonal) TYPE abap_bool.

    METHODS is_empty
      RETURNING VALUE(rv_empty) TYPE abap_bool.

    METHODS is_single_item
      RETURNING VALUE(rv_single) TYPE abap_bool.

    METHODS size
      RETURNING VALUE(rv_size) TYPE i.

    METHODS get
      IMPORTING iv_row        TYPE i
                iv_col        TYPE i
      RETURNING VALUE(rv_val) TYPE ty_matrix_value.

    METHODS set
      IMPORTING iv_row TYPE i
                iv_col TYPE i
                iv_val TYPE ty_matrix_value.

    METHODS get_row
      IMPORTING iv_row         TYPE i
      RETURNING VALUE(rt_vals) TYPE ty_matrix_values.

    METHODS get_col
      IMPORTING iv_col         TYPE i
      RETURNING VALUE(rt_vals) TYPE ty_matrix_values.

    METHODS get_rows
      RETURNING VALUE(rv_rows) TYPE i.

    METHODS get_cols
      RETURNING VALUE(rv_cols) TYPE i.

    " Structure
    METHODS reshape
      IMPORTING iv_rows          TYPE i
                iv_cols          TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS swap_row
      IMPORTING iv_row1          TYPE i
                iv_row2          TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS swap_col
      IMPORTING iv_col1          TYPE i
                iv_col2          TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS transpose_matrix
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS merge
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS clear
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS copy
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    METHODS is_same
      IMPORTING io_matrix      TYPE REF TO zcl_matrix
      RETURNING VALUE(rv_same) TYPE abap_bool.

    METHODS is_equals
      IMPORTING io_matrix        TYPE REF TO zcl_matrix
      RETURNING VALUE(rv_equals) TYPE abap_bool.

    " İnstance
    CLASS-METHODS full
      IMPORTING iv_rows          TYPE i
                iv_cols          TYPE i
                iv_vals          TYPE ty_matrix_value
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    CLASS-METHODS ones
      IMPORTING iv_rows          TYPE i
                iv_cols          TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    CLASS-METHODS zeros
      IMPORTING iv_rows          TYPE i
                iv_cols          TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    CLASS-METHODS diagonal
      IMPORTING iv_rows          TYPE i
                iv_cols          TYPE i
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

    CLASS-METHODS from_table
      IMPORTING it_matx          TYPE ty_matrix_tab
      RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix.

  PRIVATE SECTION.
    METHODS constructor
      IMPORTING iv_rows TYPE i
                iv_cols TYPE i
                it_matx TYPE ty_matrix_tab.

    DATA matrix TYPE ty_matrix_tab.
    DATA rows   TYPE i.
    DATA cols   TYPE i.
ENDCLASS.



CLASS ZCL_MATRIX IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [--->] IV_COL                         TYPE        I
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add.
    set( iv_row = iv_row
         iv_col = iv_col
         iv_val = get( iv_row = iv_row
                       iv_col = iv_col ) + iv_val ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->ADD_BY_SCALAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_by_scalar.
    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      <fs_matrix>-val = <fs_matrix>-val + iv_val.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->ADD_MATRIX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_matrix.
    DATA lv_result TYPE ty_matrix_value.

    IF NOT me->is_same( io_matrix = io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      lv_result = io_matrix->get( iv_row = <fs_matrix>-row
                                  iv_col = <fs_matrix>-col ).
      <fs_matrix>-val = <fs_matrix>-val + lv_result.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->CLEAR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD clear.
    ro_matrix = copy( ).
    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      CLEAR <fs_matrix>-val.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MATRIX->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROWS                        TYPE        I
* | [--->] IV_COLS                        TYPE        I
* | [--->] IT_MATX                        TYPE        TY_MATRIX_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    IF iv_rows <= 0 OR iv_cols <= 0.
      zcx_matrix=>raise( zcx_matrix=>c_err_matrix_size_empty ).
    ENDIF.

    rows   = iv_rows.
    cols   = iv_cols.
    matrix = it_matx.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->COPY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD copy.
    ro_matrix = NEW zcl_matrix( iv_rows = rows
                                iv_cols = cols
                                it_matx = matrix ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MATRIX=>DIAGONAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROWS                        TYPE        I
* | [--->] IV_COLS                        TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD diagonal.
    ro_matrix = zeros( iv_rows = iv_rows
                       iv_cols = iv_cols ).
    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      IF <fs_matrix>-row = <fs_matrix>-col.
        <fs_matrix>-val = 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->DIV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [--->] IV_COL                         TYPE        I
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD div.
    IF iv_val = 0.
      zcx_matrix=>raise( iv_message = zcx_matrix=>c_err_division_by_zero ).
    ENDIF.

    set( iv_row = iv_row
         iv_col = iv_col
         iv_val = get( iv_row = iv_row
                       iv_col = iv_col ) / iv_val ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->DIV_BY_SCALAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD div_by_scalar.
    IF iv_val = 0.
      zcx_matrix=>raise( iv_message = zcx_matrix=>c_err_division_by_zero ).
    ENDIF.

    ro_matrix = copy( ).
    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      <fs_matrix>-val = <fs_matrix>-val / iv_val.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->DIV_MATRIX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD div_matrix.
    DATA lv_value TYPE ty_matrix_value.

    IF NOT me->is_same( io_matrix = io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      lv_value = io_matrix->get( iv_row = <fs_matrix>-row
                                 iv_col = <fs_matrix>-col ).
      IF lv_value = 0.
        zcx_matrix=>raise( iv_message = zcx_matrix=>c_err_division_by_zero ).
      ENDIF.
      <fs_matrix>-val = <fs_matrix>-val / lv_value.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->DOT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dot.
    DATA lv_value TYPE ty_matrix_value.

    IF NOT me->is_same( io_matrix = io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    ro_matrix = zcl_matrix=>zeros( iv_rows = rows
                                   iv_cols = cols ).

    DO rows TIMES.
      DATA(i) = sy-index.
      DO cols TIMES.
        DATA(j) = sy-index.
        lv_value = 0.
        DO cols TIMES.
          DATA(k) = sy-index.
          lv_value = lv_value
                   + ( get( iv_row = i
                            iv_col = k ) * io_matrix->get( iv_row = k
                                                           iv_col = j ) ).
        ENDDO.
        ro_matrix->set( iv_row = i
                        iv_col = j
                        iv_val = lv_value ).
      ENDDO.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->DOT_COL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [--->] IV_COL                         TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dot_col.
    DATA lv_value TYPE ty_matrix_value.

    IF NOT me->is_same( io_matrix = io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    ro_matrix = zcl_matrix=>zeros( iv_rows = rows
                                   iv_cols = 1 ).

    DO rows TIMES.
      DATA(i) = sy-index.

      lv_value = 0.

      DO me->cols TIMES.
        DATA(k) = sy-index.

        lv_value = lv_value + ( me->get( iv_row = i
                                         iv_col = k ) * io_matrix->get( iv_row = i
                                                                        iv_col = iv_col ) ).
      ENDDO.

      ro_matrix->set( iv_row = i
                      iv_col = 1
                      iv_val = lv_value ).
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->DOT_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [--->] IV_ROW                         TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dot_row.
    DATA lv_value TYPE ty_matrix_value.

    IF NOT me->is_same( io_matrix = io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    ro_matrix = zcl_matrix=>zeros( iv_rows = 1
                                   iv_cols = cols ).

    DO io_matrix->cols TIMES.
      DATA(i) = sy-index.

      lv_value = 0.

      DO me->cols TIMES.
        DATA(k) = sy-index.

        lv_value = lv_value + ( get( iv_row = iv_row
                                     iv_col = k ) * io_matrix->get( iv_row = k
                                                                    iv_col = i ) ).
      ENDDO.

      ro_matrix->set( iv_row = 1
                      iv_col = i
                      iv_val = lv_value ).
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MATRIX=>FROM_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_MATX                        TYPE        TY_MATRIX_TAB
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD from_table.
    DATA lv_rows TYPE i.
    DATA lv_cols TYPE i.
    DATA lt_matx TYPE ty_matrix_tab.

    lt_matx = it_matx.

    SORT lt_matx STABLE BY row DESCENDING
                           col DESCENDING.
    " Find max row column size
    LOOP AT lt_matx ASSIGNING FIELD-SYMBOL(<fs_matx>).
      lv_rows = <fs_matx>-row.
      lv_cols = <fs_matx>-col.
      EXIT.
    ENDLOOP.

    ro_matrix = zcl_matrix=>zeros( iv_rows = lv_rows
                                   iv_cols = lv_cols ).

    LOOP AT it_matx ASSIGNING <fs_matx>.
      ro_matrix->set( iv_row = <fs_matx>-row
                      iv_col = <fs_matx>-col
                      iv_val = <fs_matx>-val ).
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MATRIX=>FULL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROWS                        TYPE        I
* | [--->] IV_COLS                        TYPE        I
* | [--->] IV_VALS                        TYPE        TY_MATRIX_VALUE
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD full.
    DATA lt_matrix TYPE ty_matrix_tab.

    lt_matrix = VALUE #( FOR lv_row_pos = 1 UNTIL lv_row_pos > iv_rows
                         FOR lv_col_pos = 1 UNTIL lv_col_pos > iv_cols
                         ( row = lv_row_pos col = lv_col_pos val = iv_vals ) ).

    ro_matrix = NEW zcl_matrix( iv_rows = iv_rows
                                iv_cols = iv_cols
                                it_matx = lt_matrix ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [--->] IV_COL                         TYPE        I
* | [<-()] RV_VAL                         TYPE        TY_MATRIX_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get.
    ASSIGN matrix[ row = iv_row
                   col = iv_col ] TO FIELD-SYMBOL(<fs_matrix>).
    IF sy-subrc <> 0.
      zcx_matrix=>raise( iv_message    = zcx_matrix=>c_err_invalid_row_col
                         iv_message_v1 = iv_row
                         iv_message_v2 = iv_col ).
    ENDIF.
    rv_val = <fs_matrix>-val.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->GET_COL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COL                         TYPE        I
* | [<-()] RT_VALS                        TYPE        TY_MATRIX_VALUES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_col.
    LOOP AT matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>) WHERE col = iv_col.
      APPEND <fs_matrix>-val TO rt_vals.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->GET_COLS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_COLS                        TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_cols.
    rv_cols = cols.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->GET_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [<-()] RT_VALS                        TYPE        TY_MATRIX_VALUES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_row.
    LOOP AT matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>) WHERE row = iv_row.
      APPEND <fs_matrix>-val TO rt_vals.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->GET_ROWS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ROWS                        TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_rows.
    rv_rows = rows.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_COL_VECTOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_VECTOR                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_col_vector.
    rv_vector = boolc( rows > 1 AND cols = 1 ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_DIAGONAL
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_DIAGONAL                    TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_diagonal.
    rv_diagonal = abap_false.

    IF NOT me->is_square( ).
      RETURN.
    ENDIF.

    LOOP AT matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      IF <fs_matrix>-row = <fs_matrix>-col.
        IF <fs_matrix>-val IS INITIAL.
          RETURN.
        ENDIF.
      ELSE.
        IF <fs_matrix>-val IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.

    rv_diagonal = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_EMPTY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_EMPTY                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_empty.
    LOOP AT matrix TRANSPORTING NO FIELDS WHERE val IS NOT INITIAL.
      EXIT.
    ENDLOOP.
    rv_empty = boolc( sy-subrc <> 0 ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_EQUALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RV_EQUALS                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_equals.
    DATA lv_row_pos TYPE i.
    DATA lv_col_pos TYPE i.

    rv_equals = abap_false.

    IF NOT me->is_same( io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    DO me->rows TIMES.
      lv_row_pos = sy-index.
      DO me->cols TIMES.
        lv_col_pos = sy-index.
        IF me->get( iv_row = lv_row_pos
                    iv_col = lv_col_pos )
           <> io_matrix->get( iv_row = lv_row_pos
                              iv_col = lv_col_pos ).
          RETURN.
        ENDIF.
      ENDDO.
    ENDDO.

    rv_equals = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_RECTANGLE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RECTANGLE                   TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_rectangle.
    rv_rectangle = boolc( ( rows > 1 AND cols > 1 ) AND rows <> cols ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_ROW_VECTOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_VECTOR                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_row_vector.
    rv_vector = boolc( rows = 1 AND cols > 1 ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_SAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RV_SAME                        TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_same.
    rv_same = abap_false.
    IF    me->get_rows( ) <> io_matrix->get_rows( )
       OR me->get_cols( ) <> io_matrix->get_cols( ).
      RETURN.
    ENDIF.
    rv_same = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_SINGLE_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SINGLE                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_single_item.
    rv_single = boolc( rows = 1 AND cols = 1 ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_SQUARE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SQUARE                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_square.
    rv_square = boolc( me->get_rows( ) = get_cols( ) ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->IS_VECTOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_VECTOR                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_vector.
    rv_vector = boolc( me->is_col_vector( ) OR me->is_row_vector( ) ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->MERGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD merge.
    DATA lv_row_pos TYPE i.
    DATA lv_col_pos TYPE i.

    ro_matrix = copy( ).

    DO io_matrix->get_rows( ) TIMES.
      lv_row_pos = sy-index.
      DO io_matrix->get_cols( ) TIMES.
        lv_col_pos = sy-index.
        INSERT VALUE #( row = lv_row_pos
                        col = lv_col_pos
                        val = io_matrix->get( iv_row = lv_row_pos + me->rows
                                              iv_col = lv_col_pos + me->cols ) )
               INTO TABLE ro_matrix->matrix.
      ENDDO.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->MUL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [--->] IV_COL                         TYPE        I
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD mul.
    set( iv_row = iv_row
         iv_col = iv_col
         iv_val = get( iv_row = iv_row
                       iv_col = iv_col ) * iv_val ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->MUL_BY_SCALAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD mul_by_scalar.
    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      <fs_matrix>-val = <fs_matrix>-val * iv_val.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->MUL_MATRIX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD mul_matrix.
    DATA lv_result TYPE ty_matrix_value.

    IF NOT me->is_same( io_matrix = io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      lv_result = io_matrix->get( iv_row = <fs_matrix>-row
                                  iv_col = <fs_matrix>-col ).
      <fs_matrix>-val = <fs_matrix>-val * lv_result.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MATRIX=>ONES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROWS                        TYPE        I
* | [--->] IV_COLS                        TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ones.
    ro_matrix = full( iv_rows = iv_rows
                      iv_cols = iv_cols
                      iv_vals = 1 ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->RESHAPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROWS                        TYPE        I
* | [--->] IV_COLS                        TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD reshape.
    DATA lv_row_pos TYPE i.
    DATA lv_col_pos TYPE i.
    DATA lv_row_add TYPE i.
    DATA lv_col_add TYPE i.

    IF iv_rows <= 0 OR iv_cols <= 0.
      zcx_matrix=>raise( zcx_matrix=>c_err_matrix_size_empty ).
    ENDIF.

    ro_matrix = copy( ).

    IF iv_rows < rows OR iv_cols < cols.
      DELETE ro_matrix->matrix WHERE row > iv_rows OR col > iv_cols.
    ENDIF.

    IF iv_rows > rows.
      lv_row_add = iv_rows + me->rows.
      DO lv_row_add TIMES.
        lv_row_pos = sy-index + me->rows.
        DO me->cols TIMES.
          lv_col_pos = sy-index.
          INSERT VALUE #( row = lv_row_pos
                          col = lv_col_pos )
                 INTO TABLE ro_matrix->matrix.
        ENDDO.
      ENDDO.
    ENDIF.

    ro_matrix->rows = iv_rows.

    IF iv_cols > cols.
      lv_col_add = iv_rows + me->rows.
      DO lv_col_add TIMES.
        lv_col_pos = sy-index + me->cols.
        DO me->rows TIMES.
          lv_row_pos = sy-index.
          INSERT VALUE #( row = lv_row_pos
                          col = lv_col_pos )
                 INTO TABLE ro_matrix->matrix.
        ENDDO.
      ENDDO.
    ENDIF.

    ro_matrix->cols = iv_cols.

    SORT ro_matrix->matrix STABLE BY row
                                     col.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [--->] IV_COL                         TYPE        I
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set.
    ASSIGN matrix[ row = iv_row
                   col = iv_col ] TO FIELD-SYMBOL(<fs_matrix>).
    IF sy-subrc <> 0.
      zcx_matrix=>raise( iv_message    = zcx_matrix=>c_err_invalid_row_col
                         iv_message_v1 = iv_row
                         iv_message_v2 = iv_col ).
    ENDIF.
    <fs_matrix>-val = iv_val.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->SIZE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SIZE                        TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD size.
    rv_size = rows * cols.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->SUB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [--->] IV_COL                         TYPE        I
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sub.
    set( iv_row = iv_row
         iv_col = iv_col
         iv_val = get( iv_row = iv_row
                       iv_col = iv_col ) - iv_val ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->SUB_BY_SCALAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VAL                         TYPE        TY_MATRIX_VALUE
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sub_by_scalar.
    ro_matrix = copy( ).
    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      <fs_matrix>-val = <fs_matrix>-val - iv_val.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->SUB_MATRIX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MATRIX                      TYPE REF TO ZCL_MATRIX
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sub_matrix.
    DATA lv_result TYPE ty_matrix_value.

    IF NOT me->is_same( io_matrix = io_matrix ).
      zcx_matrix=>raise( zcx_matrix=>c_err_inconsistent_matrix ).
    ENDIF.

    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      lv_result = io_matrix->get( iv_row = <fs_matrix>-row
                                  iv_col = <fs_matrix>-col ).
      <fs_matrix>-val = <fs_matrix>-val - lv_result.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->SWAP_COL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COL1                        TYPE        I
* | [--->] IV_COL2                        TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD swap_col.
    DATA lv_value TYPE ty_matrix_value.

    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix1>) WHERE col = iv_col1.
      LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix2>) WHERE col = iv_col2.
        lv_value         = <fs_matrix1>-val.
        <fs_matrix1>-val = <fs_matrix2>-val.
        <fs_matrix2>-val = lv_value.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->SWAP_ROW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW1                        TYPE        I
* | [--->] IV_ROW2                        TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD swap_row.
    DATA lv_value TYPE ty_matrix_value.

    ro_matrix = copy( ).

    LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix1>) WHERE row = iv_row1.
      LOOP AT ro_matrix->matrix ASSIGNING FIELD-SYMBOL(<fs_matrix2>) WHERE row = iv_row2.
        lv_value         = <fs_matrix1>-val.
        <fs_matrix1>-val = <fs_matrix2>-val.
        <fs_matrix2>-val = lv_value.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MATRIX->TRANSPOSE_MATRIX
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD transpose_matrix.
    DATA lt_transposed TYPE ty_matrix_tab.

    LOOP AT matrix ASSIGNING FIELD-SYMBOL(<fs_matrix>).
      INSERT VALUE #( col = <fs_matrix>-row
                      row = <fs_matrix>-col
                      val = <fs_matrix>-val )
             INTO TABLE lt_transposed.
    ENDLOOP.

    ro_matrix = NEW #( iv_rows = rows
                       iv_cols = cols
                       it_matx = lt_transposed ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MATRIX=>ZEROS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROWS                        TYPE        I
* | [--->] IV_COLS                        TYPE        I
* | [<-()] RO_MATRIX                      TYPE REF TO ZCL_MATRIX
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zeros.
    ro_matrix = full( iv_rows = iv_rows
                      iv_cols = iv_cols
                      iv_vals = 0 ).
  ENDMETHOD.
ENDCLASS.