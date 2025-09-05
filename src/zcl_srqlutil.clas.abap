class ZCL_SRQLUTIL definition
  public
  final
  create public .

public section.

  class-methods GET_FIELD
    importing
      value(LINE) type ANY
      value(FIELDNAME) type FIELDNAME
    exporting
      value(VALUE) type ANY
    raising
      ZCX_SRQLTYPE .
  class-methods MOVE_CORRESPONDING
    importing
      value(LINEFROM) type ANY
    exporting
      value(MOVED) type I
    changing
      !LINETO type ANY
    raising
      ZCX_SRQLTYPE .
  class-methods MOVE_FIELD
    importing
      value(LINEFROM) type ANY
      value(FIELDFROM) type FIELDNAME
      value(FIELDTO) type FIELDNAME
    changing
      !LINETO type ANY
    raising
      ZCX_SRQLTYPE .
  class-methods MOVE_MAPPING
    importing
      value(LINEFROM) type ANY
      value(FIELDMAP) type ZSRQLFIELDMAP_S_TT
    exporting
      value(MOVED) type I
    changing
      !LINETO type ANY
    raising
      ZCX_SRQLTYPE .
  class-methods SET_FIELD
    importing
      value(FIELDNAME) type FIELDNAME
      value(VALUE) type ANY
    changing
      !LINE type ANY
    raising
      ZCX_SRQLTYPE .
  class-methods SPLIT_TEXT
    importing
      value(I_TEXT) type STRING
      value(I_LENGTH) type I default 72
    exporting
      value(O_TEXT) type CSEQUENCE
    returning
      value(R_TEXT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SRQLUTIL IMPLEMENTATION.


method get_field.

  data: lx_root type ref to cx_root.

  field-symbols: <lv_field> type any.

  try.

*     get field
      assign component fieldname of structure line to <lv_field>.
      if sy-subrc ne 0. raise exception type zcx_srqltype exporting textid = zcx_srqltype=>zcx_srqltype_field fieldname = fieldname. endif.

*     return value
      value = <lv_field>.

    catch cx_sy_assign_cast_illegal_cast cx_sy_assign_cast_unknown_type cx_sy_assign_out_of_range into lx_root.

      raise exception type zcx_srqltype
        exporting
          textid    = zcx_srqltype=>zcx_srqltype_field
          previous  = lx_root
          fieldname = fieldname.

  endtry.

endmethod.


method move_corresponding.

  data: lx_root   type ref to cx_root,
        lo_struc1 type ref to cl_abap_structdescr,
        lo_struc2 type ref to cl_abap_structdescr,
        lt_comp1  type cl_abap_structdescr=>component_table,
        lt_comp2  type cl_abap_structdescr=>component_table,
        ls_comp1  type cl_abap_structdescr=>component,
        ls_comp2  type cl_abap_structdescr=>component,
        lv_from   type fieldname,
        lv_to     type fieldname.

  field-symbols: <lv_field1> type any,
                 <lv_field2> type any.

* describe local structure
  lo_struc1 ?= cl_abap_structdescr=>describe_by_data( linefrom ).
  lo_struc2 ?= cl_abap_structdescr=>describe_by_data( lineto ).

* get local structure components
  lt_comp1 = lo_struc1->get_components( ).
  lt_comp2 = lo_struc2->get_components( ).

  try.

*     map components
      loop at lt_comp1 into ls_comp1.
*       unassign 4 loop
        if <lv_field1> is assigned. unassign <lv_field1>. endif.
        if <lv_field2> is assigned. unassign <lv_field2>. endif.
*       get corresponding field
        read table lt_comp2 into ls_comp2 with key name = ls_comp1-name.
        if sy-subrc ne 0. continue. endif.
*       get fields
        assign component: ls_comp1-name of structure linefrom to <lv_field1>, ls_comp2-name of structure lineto   to <lv_field2>.
        if not ( <lv_field2> is assigned and <lv_field1> is assigned ). continue. endif.
*       move field value
        <lv_field2> = <lv_field1>.
        add 1 to moved.
      endloop.

    catch cx_sy_assign_cast_illegal_cast cx_sy_assign_cast_unknown_type cx_sy_assign_out_of_range into lx_root.

      lv_from = ls_comp1-name. lv_to = ls_comp2-name.

      raise exception type zcx_srqltype
        exporting
          textid    = zcx_srqltype=>zcx_srqltype_field_move
          previous  = lx_root
          fieldname = lv_from
          fieldto   = lv_to.

  endtry.

endmethod.


method move_field.

  data: lx_root type ref to cx_root.

  field-symbols: <lv_from> type any,
                 <lv_to>   type any.

  try.

*     get field from
      assign component fieldfrom of structure linefrom to <lv_from>.
      if sy-subrc ne 0. raise exception type zcx_srqltype exporting textid = zcx_srqltype=>zcx_srqltype_field fieldname = fieldfrom. endif.

*     get field to
      assign component fieldto of structure lineto to <lv_to>.
      if sy-subrc ne 0. raise exception type zcx_srqltype exporting textid = zcx_srqltype=>zcx_srqltype_field fieldname = fieldto. endif.

*     set value
      <lv_to> = <lv_from>.

    catch cx_sy_assign_cast_illegal_cast cx_sy_assign_cast_unknown_type cx_sy_assign_out_of_range into lx_root.

      raise exception type zcx_srqltype
        exporting
          textid    = zcx_srqltype=>zcx_srqltype_field_move
          previous  = lx_root
          fieldname = fieldfrom
          fieldto   = fieldto.

  endtry.

endmethod.


method move_mapping.

  data: lx_root type ref to cx_root,
        ls_map  type zsrqlfieldmap_s.

  field-symbols: <lv_field1> type any,
                 <lv_field2> type any.

  try.

      loop at fieldmap into ls_map.
*       unassign 4 loop.
        if <lv_field1> is assigned. unassign <lv_field1>. endif.
        if <lv_field2> is assigned. unassign <lv_field2>. endif.
*       get fields
        assign component: ls_map-fieldfrom of structure linefrom to <lv_field1>, ls_map-fieldto of structure lineto to <lv_field2>.
        if not ( <lv_field2> is assigned and <lv_field1> is assigned ). continue. endif.
*       move value
        <lv_field2> = <lv_field1>.
        add 1 to moved.
      endloop.

    catch cx_sy_assign_cast_illegal_cast cx_sy_assign_cast_unknown_type cx_sy_assign_out_of_range into lx_root.

      raise exception type zcx_srqltype
        exporting
          textid    = zcx_srqltype=>zcx_srqltype_field_move
          previous  = lx_root
          fieldname = ls_map-fieldfrom
          fieldto   = ls_map-fieldto.

  endtry.

endmethod.


method set_field.

  data: lx_root type ref to cx_root.

  field-symbols: <lv_field> type any.

  try.

*     get field
      assign component fieldname of structure line to <lv_field>.
      if sy-subrc ne 0. raise exception type zcx_srqltype exporting textid = zcx_srqltype=>zcx_srqltype_field fieldname = fieldname. endif.

*     set value
      <lv_field> = value.

    catch cx_sy_assign_cast_illegal_cast cx_sy_assign_cast_unknown_type cx_sy_assign_out_of_range into lx_root.

      raise exception type zcx_srqltype
        exporting
          textid    = zcx_srqltype=>zcx_srqltype_field
          previous  = lx_root
          fieldname = fieldname.

  endtry.

endmethod.


  METHOD split_text.
    DATA ld_text TYPE string.
    DATA lt_results TYPE  match_result_tab.
    ld_text = i_text.
    IF strlen( ld_text ) <= i_length.
      r_text = ld_text.
      CLEAR o_text.
      RETURN.
    ENDIF.
    ld_text = ld_text(i_length).
    FIND ALL OCCURRENCES OF REGEX '\s' IN ld_text RESULTS lt_results.
    IF lines( lt_results ) > 0.
      SORT lt_results BY offset DESCENDING.
      ASSIGN lt_results[ 1 ] TO FIELD-SYMBOL(<lf_result>).
      IF <lf_result> IS ASSIGNED.
        r_text = ld_text(<lf_result>-offset).
        o_text = i_text+<lf_result>-offset.
        RETURN.
      ENDIF.
    ELSE.
      r_text = ld_text.
      CLEAR o_text.
    ENDIF.

    r_text = ld_text.
    CLEAR o_text.

  ENDMETHOD.
ENDCLASS.
