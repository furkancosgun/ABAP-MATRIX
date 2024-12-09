*"* use this source file for your ABAP unit test classes

CLASS ltcl_matrix_math DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS test_add        FOR TESTING.
    METHODS test_sub        FOR TESTING.
    METHODS test_mul        FOR TESTING.
    METHODS test_div        FOR TESTING.
    METHODS test_add_scalar FOR TESTING.
    METHODS test_sub_scalar FOR TESTING.
    METHODS test_mul_scalar FOR TESTING.
    METHODS test_div_scalar FOR TESTING.
    METHODS test_add_matrix FOR TESTING.
    METHODS test_sub_matrix FOR TESTING.
    METHODS test_mul_matrix FOR TESTING.
    METHODS test_div_matrix FOR TESTING.

  PRIVATE SECTION.
    METHODS setup.

    DATA matrix TYPE REF TO zcl_matrix.
ENDCLASS.


CLASS ltcl_matrix_math IMPLEMENTATION.
  METHOD setup.
    matrix = zcl_matrix=>from_table( it_matx = VALUE #( ( row = 1 col = 1 val = 1 )
                                                        ( row = 1 col = 2 val = 2 )
                                                        ( row = 2 col = 1 val = 3 )
                                                        ( row = 2 col = 2 val = 4 )
                                                        ( row = 3 col = 1 val = 5 )
                                                        ( row = 3 col = 2 val = 6 ) ) ).
  ENDMETHOD.

  METHOD test_add.
    DATA lv_row TYPE i VALUE 1.
    DATA lv_col TYPE i VALUE 1.

    matrix->add( iv_row = lv_row
                 iv_col = lv_col
                 iv_val = 10 ).

    cl_abap_unit_assert=>assert_equals( exp = 11
                                        act = matrix->get( iv_row = lv_row
                                                           iv_col = lv_col )  ).
  ENDMETHOD.

  METHOD test_sub.
    DATA lv_row TYPE i VALUE 1.
    DATA lv_col TYPE i VALUE 2.

    matrix->sub( iv_row = lv_row
                 iv_col = lv_col
                 iv_val = 2 ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = matrix->get( iv_row = lv_row
                                                           iv_col = lv_col )  ).
  ENDMETHOD.

  METHOD test_mul.
    DATA lv_row TYPE i VALUE 2.
    DATA lv_col TYPE i VALUE 1.

    matrix->mul( iv_row = lv_row
                 iv_col = lv_col
                 iv_val = 4 ).

    cl_abap_unit_assert=>assert_equals( exp = 12
                                        act = matrix->get( iv_row = lv_row
                                                           iv_col = lv_col )  ).
  ENDMETHOD.

  METHOD test_div.
    DATA lv_row TYPE i VALUE 2.
    DATA lv_col TYPE i VALUE 2.

    matrix->div( iv_row = lv_row
                 iv_col = lv_col
                 iv_val = 4 ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = matrix->get( iv_row = lv_row
                                                           iv_col = lv_col )  ).
    TRY.
        matrix->div( iv_row = lv_row
                     iv_col = lv_col
                     iv_val = 0 ).

        " Zero division not allowed
        cl_abap_unit_assert=>fail( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_scalar.
    DATA lv_val TYPE zcl_matrix=>ty_matrix_value.

    DATA(lt_rows) = matrix->get_row( iv_row = 1 ).
    DATA(lt_cols) = matrix->get_col( iv_col = 1 ).

    DATA(lo_test) = matrix->add_by_scalar( iv_val = 1 ).

    LOOP AT lt_rows INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val + 1
                                          act = lo_test->get( iv_row = 1
                                                              iv_col = sy-tabix )  ).
    ENDLOOP.
    LOOP AT lt_cols INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val + 1
                                          act = lo_test->get( iv_row = sy-tabix
                                                              iv_col = 1 )  ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_sub_scalar.
    DATA lv_val TYPE zcl_matrix=>ty_matrix_value.

    DATA(lt_rows) = matrix->get_row( iv_row = 1 ).
    DATA(lt_cols) = matrix->get_col( iv_col = 1 ).

    DATA(lo_test) = matrix->sub_by_scalar( iv_val = 1 ).

    LOOP AT lt_rows INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val - 1
                                          act = lo_test->get( iv_row = 1
                                                              iv_col = sy-tabix )  ).
    ENDLOOP.
    LOOP AT lt_cols INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val - 1
                                          act = lo_test->get( iv_row = sy-tabix
                                                              iv_col = 1 )  ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_mul_scalar.
    DATA lv_val TYPE zcl_matrix=>ty_matrix_value.

    DATA(lt_rows) = matrix->get_row( iv_row = 1 ).
    DATA(lt_cols) = matrix->get_col( iv_col = 1 ).

    DATA(lo_test) = matrix->mul_by_scalar( iv_val = 2 ).

    LOOP AT lt_rows INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val * 2
                                          act = lo_test->get( iv_row = 1
                                                              iv_col = sy-tabix )  ).
    ENDLOOP.
    LOOP AT lt_cols INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val * 2
                                          act = lo_test->get( iv_row = sy-tabix
                                                              iv_col = 1 )  ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_div_scalar.
    DATA lv_val TYPE zcl_matrix=>ty_matrix_value.

    DATA(lt_rows) = matrix->get_row( iv_row = 1 ).
    DATA(lt_cols) = matrix->get_col( iv_col = 1 ).

    DATA(lo_test) = matrix->div_by_scalar( iv_val = 2 ).

    LOOP AT lt_rows INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val / 2
                                          act = lo_test->get( iv_row = 1
                                                              iv_col = sy-tabix )  ).
    ENDLOOP.
    LOOP AT lt_cols INTO lv_val.
      cl_abap_unit_assert=>assert_equals( exp = lv_val / 2
                                          act = lo_test->get( iv_row = sy-tabix
                                                              iv_col = 1 )  ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_add_matrix.
    DATA(lo_other_matrix) = matrix->copy( ).
    DATA(lo_test) = matrix->add_matrix( io_matrix = lo_other_matrix ).

    DO matrix->get_rows( ) TIMES.
      DATA(lv_row_pos) = sy-index.
      DO matrix->get_cols( ) TIMES.
        DATA(lv_col_pos) = sy-index.
        DATA(lv_exp) = matrix->get( iv_row = lv_row_pos
                                    iv_col = lv_col_pos ) + lo_other_matrix->get( iv_row = lv_row_pos
                                                                                  iv_col = lv_col_pos ).
        cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                            act = lo_test->get( iv_row = lv_row_pos
                                                                iv_col = lv_col_pos )  ).
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD test_sub_matrix.
    DATA(lo_other_matrix) = matrix->copy( ).
    DATA(lo_test) = matrix->sub_matrix( io_matrix = lo_other_matrix ).

    DO matrix->get_rows( ) TIMES.
      DATA(lv_row_pos) = sy-index.
      DO matrix->get_cols( ) TIMES.
        DATA(lv_col_pos) = sy-index.
        DATA(lv_exp) = matrix->get( iv_row = lv_row_pos
                                    iv_col = lv_col_pos ) - lo_other_matrix->get( iv_row = lv_row_pos
                                                                                  iv_col = lv_col_pos ).
        cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                            act = lo_test->get( iv_row = lv_row_pos
                                                                iv_col = lv_col_pos )  ).
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD test_mul_matrix.
    DATA(lo_other_matrix) = matrix->copy( ).
    DATA(lo_test) = matrix->mul_matrix( io_matrix = lo_other_matrix ).

    DO matrix->get_rows( ) TIMES.
      DATA(lv_row_pos) = sy-index.
      DO matrix->get_cols( ) TIMES.
        DATA(lv_col_pos) = sy-index.
        DATA(lv_exp) = matrix->get( iv_row = lv_row_pos
                                    iv_col = lv_col_pos ) * lo_other_matrix->get( iv_row = lv_row_pos
                                                                                  iv_col = lv_col_pos ).
        cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                            act = lo_test->get( iv_row = lv_row_pos
                                                                iv_col = lv_col_pos )  ).
      ENDDO.
    ENDDO.
  ENDMETHOD.

  METHOD test_div_matrix.
    DATA(lo_other_matrix) = matrix->copy( ).
    DATA(lo_test) = matrix->div_matrix( io_matrix = lo_other_matrix ).

    DO matrix->get_rows( ) TIMES.
      DATA(lv_row_pos) = sy-index.
      DO matrix->get_cols( ) TIMES.
        DATA(lv_col_pos) = sy-index.
        DATA(lv_exp) = matrix->get( iv_row = lv_row_pos
                                    iv_col = lv_col_pos ) / lo_other_matrix->get( iv_row = lv_row_pos
                                                                                  iv_col = lv_col_pos ).
        cl_abap_unit_assert=>assert_equals( exp = lv_exp
                                            act = lo_test->get( iv_row = lv_row_pos
                                                                iv_col = lv_col_pos )  ).
      ENDDO.
    ENDDO.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_matrix_prop DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS test_is_square      FOR TESTING.
    METHODS test_is_rectangle   FOR TESTING.
    METHODS test_is_row_vector  FOR TESTING.
    METHODS test_is_col_vector  FOR TESTING.
    METHODS test_is_vector      FOR TESTING.
    METHODS test_is_diagonal    FOR TESTING.
    METHODS test_is_empty       FOR TESTING.
    METHODS test_is_single_item FOR TESTING.
    METHODS test_size           FOR TESTING.

  PRIVATE SECTION.
    METHODS setup.

    DATA matrix TYPE REF TO zcl_matrix.
ENDCLASS.


CLASS ltcl_matrix_prop IMPLEMENTATION.
  METHOD setup.
    matrix = zcl_matrix=>from_table( it_matx = VALUE #( ( row = 1 col = 1 val = 1 )
                                                        ( row = 1 col = 2 val = 2 )
                                                        ( row = 2 col = 1 val = 3 )
                                                        ( row = 2 col = 2 val = 4 )
                                                        ( row = 3 col = 1 val = 5 )
                                                        ( row = 3 col = 2 val = 6 ) ) ).
  ENDMETHOD.

  METHOD test_is_col_vector.
    cl_abap_unit_assert=>assert_false( act = matrix->is_col_vector( ) ).

    DATA(lo_matrix) = matrix->reshape( iv_rows = 3
                                       iv_cols = 1 ).

    cl_abap_unit_assert=>assert_true( act = lo_matrix->is_col_vector( ) ).
  ENDMETHOD.

  METHOD test_is_diagonal.
    cl_abap_unit_assert=>assert_false( act = matrix->is_diagonal( ) ).

    DATA(lo_matrix) = zcl_matrix=>diagonal( iv_rows = 3
                                            iv_cols = 3 ).
    cl_abap_unit_assert=>assert_true( act = lo_matrix->is_diagonal( ) ).
  ENDMETHOD.

  METHOD test_is_empty.
    cl_abap_unit_assert=>assert_false( act = matrix->is_empty( ) ).

    DATA(lo_matrix) = zcl_matrix=>zeros( iv_rows = 3
                                         iv_cols = 3 ).
    cl_abap_unit_assert=>assert_true( act = lo_matrix->is_empty( ) ).
  ENDMETHOD.

  METHOD test_is_rectangle.
    cl_abap_unit_assert=>assert_true( act = matrix->is_rectangle( ) ).

    DATA(lo_matrix) = zcl_matrix=>zeros( iv_rows = 3
                                         iv_cols = 3 ).

    cl_abap_unit_assert=>assert_false( act = lo_matrix->is_rectangle( ) ).
  ENDMETHOD.

  METHOD test_is_row_vector.
    cl_abap_unit_assert=>assert_false( act = matrix->is_row_vector( ) ).

    DATA(lo_matrix) = matrix->reshape( iv_rows = 1
                                       iv_cols = 5 ).

    cl_abap_unit_assert=>assert_true( act = lo_matrix->is_row_vector( ) ).
  ENDMETHOD.

  METHOD test_is_single_item.
    cl_abap_unit_assert=>assert_false( act = matrix->is_single_item( ) ).

    DATA(lo_matrix) = matrix->reshape( iv_rows = 1
                                       iv_cols = 1 ).

    cl_abap_unit_assert=>assert_true( act = lo_matrix->is_single_item( ) ).
  ENDMETHOD.

  METHOD test_is_square.
    cl_abap_unit_assert=>assert_false( act = matrix->is_square( ) ).

    DATA(lo_matrix) = matrix->reshape( iv_rows = 5
                                       iv_cols = 5 ).

    cl_abap_unit_assert=>assert_true( act = lo_matrix->is_square( ) ).
  ENDMETHOD.

  METHOD test_is_vector.
    cl_abap_unit_assert=>assert_false( act = matrix->is_vector( ) ).

    DATA(lo_matrix) = matrix->reshape( iv_rows = 1
                                       iv_cols = 5 ).

    cl_abap_unit_assert=>assert_true( act = lo_matrix->is_vector( ) ).
  ENDMETHOD.

  METHOD test_size.
    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = matrix->size( ) ).
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_matrix_dot DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS test_dot     FOR TESTING.
    METHODS test_dot_row FOR TESTING.
    METHODS test_dot_col FOR TESTING.

  PRIVATE SECTION.
    METHODS setup.

    "[ 4,2 ]
    "[ 0,3 ]
    DATA matrix1 TYPE REF TO zcl_matrix.
    "[ 4,0 ]
    "[ 1,4 ]
    DATA matrix2 TYPE REF TO zcl_matrix.
ENDCLASS.


CLASS ltcl_matrix_dot IMPLEMENTATION.
  METHOD setup.
    matrix1 = zcl_matrix=>zeros( iv_rows = 2
                                 iv_cols = 2 ).
    matrix1->set( iv_row = 1
                  iv_col = 1
                  iv_val = 4 ).

    matrix1->set( iv_row = 1
                  iv_col = 2
                  iv_val = 2 ).

    matrix1->set( iv_row = 2
                  iv_col = 1
                  iv_val = 0 ).

    matrix1->set( iv_row = 2
                  iv_col = 2
                  iv_val = 3 ).

    matrix2 = zcl_matrix=>zeros( iv_rows = 2
                                 iv_cols = 2 ).

    matrix2->set( iv_row = 1
                  iv_col = 1
                  iv_val = 4 ).

    matrix2->set( iv_row = 1
                  iv_col = 2
                  iv_val = 0 ).

    matrix2->set( iv_row = 2
                  iv_col = 1
                  iv_val = 1 ).

    matrix2->set( iv_row = 2
                  iv_col = 2
                  iv_val = 4 ).
  ENDMETHOD.

  METHOD test_dot.
    DATA(lo_result) = matrix1->dot( io_matrix = matrix2 ).

    cl_abap_unit_assert=>assert_equals( exp = 18
                                        act = lo_result->get( iv_row = 1
                                                              iv_col = 1 )  ).

    cl_abap_unit_assert=>assert_equals( exp = 8
                                        act = lo_result->get( iv_row = 1
                                                              iv_col = 2 )  ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lo_result->get( iv_row = 2
                                                              iv_col = 1 )  ).

    cl_abap_unit_assert=>assert_equals( exp = 12
                                        act = lo_result->get( iv_row = 2
                                                              iv_col = 2 )  ).
  ENDMETHOD.

  METHOD test_dot_row.
    DATA(lo_result) = matrix1->dot_row( iv_row    = 1
                                        io_matrix = matrix2 ).

    cl_abap_unit_assert=>assert_equals( exp = 18
                                        act = lo_result->get( iv_row = 1
                                                              iv_col = 1 )  ).

    cl_abap_unit_assert=>assert_equals( exp = 8
                                        act = lo_result->get( iv_row = 1
                                                              iv_col = 2 )  ).
  ENDMETHOD.

  METHOD test_dot_col.
    DATA(lo_result) = matrix1->dot_col( iv_col    = 2
                                        io_matrix = matrix2 ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lo_result->get( iv_row = 1
                                                              iv_col = 1 )  ).

    cl_abap_unit_assert=>assert_equals( exp = 12
                                        act = lo_result->get( iv_row = 2
                                                              iv_col = 1 )  ).
  ENDMETHOD.
ENDCLASS.