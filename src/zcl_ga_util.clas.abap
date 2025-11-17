CLASS zcl_ga_util DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF opt,
        i(1)  VALUE 'I',
        e(1)  VALUE 'E',
        bt(2) VALUE 'BT',
        nb(2) VALUE 'NB',
        eq(2) VALUE 'EQ',
        ne(2) VALUE 'NE',
        gt(2) VALUE 'GT',
        ge(2) VALUE 'GE',
        lt(2) VALUE 'LT',
        le(2) VALUE 'LE',
        cp(2) VALUE 'CP',
        np(2) VALUE 'NP',
        n(40) VALUE 'NB,NE',
      END OF opt .
    CONSTANTS:
      BEGIN OF date_pattern,
        sap_pattern            TYPE string VALUE 'aaaammdd',
        no_sep_no_angl_pattern TYPE string VALUE 'ddmmaaaa',
        no_sep_angl_pattern    TYPE string VALUE 'mmddaaaa',
        sep_no_angl_pattern    TYPE string VALUE 'dd+mm+aaaa',
        sep_angl_pattern       TYPE string VALUE 'mm+dd+aaaa',
      END OF date_pattern .
    CONSTANTS:
      BEGIN OF time_pattern,
        sap_pattern TYPE string VALUE 'hhmmss',
        adp_pattern TYPE string VALUE 'hh:mm:ss tt',
      END OF time_pattern .

    "EXPORTING VALUE(op_value) TYPE any
    CLASS-METHODS apply_conv_out
      IMPORTING
        !ip_value    TYPE any
      RETURNING
        VALUE(r_out) TYPE string .
    CLASS-METHODS apply_conv         "IMPORTING VALUE(ip_value) TYPE any
      CHANGING
        !iop_value TYPE any
      RAISING
        zcx_ga_util .
    CLASS-METHODS icon_create
      IMPORTING
        VALUE(ip_icon)  TYPE any
      CHANGING
        VALUE(iop_icon) TYPE any
        VALUE(iop_text) TYPE any OPTIONAL .
    CLASS-METHODS get_sel_option_icon_name
      IMPORTING
        VALUE(ip_sign)     TYPE any
        VALUE(ip_option)   TYPE any
      RETURNING
        VALUE(r_icon_name) TYPE string .
    CLASS-METHODS get_ref_to_struct
      IMPORTING
                VALUE(ip_structure_name) TYPE string
                VALUE(ip_rfc_dest)       TYPE rfcdest OPTIONAL
                VALUE(it_fields)         TYPE esh_t_co_rfcrt_fields OPTIONAL
                VALUE(ip_table)          TYPE abap_bool DEFAULT abap_false
      RETURNING
                VALUE(op_data)           TYPE REF TO data
      RAISING   zcx_ga_util.
    CLASS-METHODS get_rfc_table
      IMPORTING
        VALUE(ip_tabname)         TYPE tabname
        VALUE(ip_rfc_dest)        TYPE rfcdest OPTIONAL
        VALUE(it_fields)          TYPE esh_t_co_rfcrt_fields OPTIONAL
        VALUE(it_where_selection) TYPE rsds_trange OPTIONAL
        VALUE(it_where)           TYPE esh_t_co_rfcrt_options OPTIONAL
        VALUE(ip_buffered)        TYPE abap_bool DEFAULT abap_false
      EXPORTING
        VALUE(op_data)            TYPE REF TO data
        VALUE(ot_message)         TYPE bapiret2_tab
      RAISING
        zcx_ga_util
      .
    CLASS-METHODS show_sel_options
      IMPORTING
        VALUE(ip_title)             TYPE sy-title OPTIONAL
        VALUE(ip_just_display)      TYPE abap_bool DEFAULT abap_false
        VALUE(is_optionlist)        TYPE rsoptions OPTIONAL
        VALUE(ip_signs_restriction) TYPE raldb-sign OPTIONAL
        VALUE(ip_no_del_line)       TYPE abap_bool DEFAULT abap_true
      CHANGING
        VALUE(iop_sign)             TYPE tvarv_sign OPTIONAL
        VALUE(iop_option)           TYPE tvarv_opti OPTIONAL
      RETURNING
        VALUE(r_ok)                 TYPE abap_bool .
    CLASS-METHODS show_complex_sel_options
      IMPORTING
        !ip_title             TYPE sy-title OPTIONAL
        !ip_text              TYPE rsseltext OPTIONAL
        !ip_signed            TYPE abap_bool DEFAULT abap_true
        !ip_lower_case        TYPE abap_bool DEFAULT abap_false
        !ip_no_interval_check TYPE abap_bool DEFAULT abap_true
        !ip_just_display      TYPE abap_bool DEFAULT abap_false
        !ip_just_incl         TYPE abap_bool DEFAULT abap_false
        !is_excluded_options  TYPE rsoptions OPTIONAL
        !ip_element_data      TYPE string DEFAULT 'STRING'
      CHANGING
        !iot_range            TYPE ANY TABLE
      RETURNING
        VALUE(r_ok)           TYPE abap_bool .
    CLASS-METHODS popup_to_confirm
      IMPORTING
        !iv_title          TYPE csequence
        !iv_question       TYPE csequence
        !iv_icon_1         TYPE icon-name DEFAULT 'ICON_OKAY'
        !iv_text_1         TYPE csequence OPTIONAL
        !iv_icon_2         TYPE icon-name DEFAULT 'ICON_CANCEL'
        !iv_text_2         TYPE csequence OPTIONAL
        !iv_default        TYPE char1 DEFAULT '2'
        !iv_display_cancel TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_ok)       TYPE abap_bool .
    CLASS-METHODS create_ref_to_range
      IMPORTING
        !ip_name_data TYPE string OPTIONAL
        !ip_data      TYPE any OPTIONAL
      RETURNING
        VALUE(ro_ref) TYPE REF TO data .
    CLASS-METHODS debug
      IMPORTING
        VALUE(ip_ms_debug_time) TYPE i DEFAULT 60        "#EC NUMBER_OK
        VALUE(ip_just_batch)    TYPE abap_bool DEFAULT abap_true
        VALUE(ip_user)          TYPE sy-uname DEFAULT sy-uname .
    CLASS-METHODS get_components
      IMPORTING
        !io_structure TYPE REF TO cl_abap_structdescr
      CHANGING
        !iot_columns  TYPE abap_component_tab .
    METHODS constructor
      IMPORTING
        VALUE(ip_logger) TYPE REF TO zcl_alog_msg_logger_base OPTIONAL .
    METHODS log_d
      IMPORTING
        VALUE(iv_text) TYPE csequence .
    METHODS log_e
      IMPORTING
        VALUE(iv_text) TYPE csequence .
    METHODS log_i
      IMPORTING
        VALUE(iv_text) TYPE csequence .
    METHODS log_w
      IMPORTING
        VALUE(iv_text) TYPE csequence .
    METHODS log_exception
      IMPORTING
        VALUE(ix_ex) TYPE REF TO cx_root .
    CLASS-METHODS get_singleton
      IMPORTING
        VALUE(ip_logger) TYPE REF TO zcl_alog_msg_logger_base OPTIONAL
      RETURNING
        VALUE(r_single)  TYPE REF TO zcl_ga_util .
    CLASS-METHODS get_lvc_fcat
      IMPORTING
        !ip_data       TYPE any
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .
    CLASS-METHODS get_slis_fcat
      IMPORTING
        !ip_data       TYPE any
      RETURNING
        VALUE(rt_fcat) TYPE slis_t_fieldcat_alv .
    CLASS-METHODS check_plausability_date
      IMPORTING
        !ip_possible_date       TYPE any
        !ip_date_pattern        TYPE string OPTIONAL
      EXPORTING
        VALUE(op_serious_doubt) TYPE abap_bool
      RETURNING
        VALUE(r_date)           TYPE sy-datum
      RAISING
        zcx_ga_util .
    CLASS-METHODS check_plausability_time
      IMPORTING
        !ip_possible_time TYPE any
        !ip_time_pattern  TYPE string DEFAULT time_pattern-sap_pattern
      EXPORTING
        VALUE(op_ok)      TYPE abap_bool
      RETURNING
        VALUE(r_time)     TYPE sy-uzeit
      RAISING
        zcx_ga_util .
    CLASS-METHODS is_subclass
      IMPORTING
        VALUE(ip_class)        TYPE seoclsname
        VALUE(ip_subclass)     TYPE seoclsname OPTIONAL
        VALUE(ip_subclass_obj) TYPE REF TO object OPTIONAL
      RETURNING
        VALUE(r_ok)            TYPE abap_bool .
    CLASS-METHODS show_messages
      IMPORTING
        VALUE(ip_title)    TYPE string OPTIONAL
        VALUE(it_messages) TYPE bapiret2_t OPTIONAL
      CHANGING
        VALUE(iot_answer)  TYPE bal_s_excm OPTIONAL .
    CLASS-METHODS read_text
      IMPORTING
        VALUE(is_head) TYPE thead
      EXPORTING
        VALUE(ot_text) TYPE bbpt_tline
      RETURNING
        VALUE(r_ok)    TYPE abap_bool .
    CLASS-METHODS get_standard_text
      IMPORTING
        VALUE(ip_tdobject) TYPE tdobject
        VALUE(ip_tdname)   TYPE tdobname
        VALUE(ip_tdid)     TYPE tdid OPTIONAL
        VALUE(ip_spras)    TYPE spras DEFAULT sy-langu
      RETURNING
        VALUE(rt_text)     TYPE bbpt_tline .
    CLASS-METHODS check_time_is_adp_pattern
      IMPORTING
        !ip_time_string TYPE string
      EXPORTING
        !op_time        TYPE t
      RETURNING
        VALUE(r_ok)     TYPE abap_bool .
    CLASS-METHODS check_time_is_sap_pattern
      IMPORTING
        !ip_time_string TYPE string
      EXPORTING
        !op_time        TYPE t
      RETURNING
        VALUE(r_ok)     TYPE abap_bool .
    CLASS-METHODS check_time_values
      CHANGING
        !iop_hour    TYPE n
        !iop_minutes TYPE n
        !iop_seconds TYPE n
        !iop_tt      TYPE c OPTIONAL
      RETURNING
        VALUE(r_ok)  TYPE abap_bool .
  PROTECTED SECTION.
    CONSTANTS: BEGIN OF icons_sel_option,
                 eq(50)   VALUE 'ICON_EQUAL_',
                 ne(50)   VALUE 'ICON_NOT_EQUAL_',
                 gt(50)   VALUE 'ICON_GREATER_',
                 lt(50)   VALUE 'ICON_LESS_',
                 ge(50)   VALUE 'ICON_GREATER_EQUAL_',
                 le(50)   VALUE 'ICON_LESS_EQUAL_',
                 bt(50)   VALUE 'ICON_INTERVAL_INCLUDE_',
                 nb(50)   VALUE 'ICON_INTERVAL_EXCLUDE_',
                 cp(50)   VALUE 'ICON_PATTERN_INCLUDE_',
                 np(50)   VALUE 'ICON_PATTERN_EXCLUDE_',
                 green(5) VALUE 'GREEN',
                 red(3)   VALUE 'RED',
               END OF icons_sel_option.
  PRIVATE SECTION.

    DATA o_logger TYPE REF TO zcl_alog_msg_logger_base .
    CLASS-DATA m_singleton TYPE REF TO zcl_ga_util .

    CLASS-METHODS get_parent_class
      IMPORTING
        VALUE(ip_class) TYPE seoclsname
      RETURNING
        VALUE(r_class)  TYPE seoclsname .
ENDCLASS.



CLASS zcl_ga_util IMPLEMENTATION.


  METHOD apply_conv.
    DATA: lo_elem       TYPE REF TO cl_abap_elemdescr,
          lo_type       TYPE REF TO cl_abap_typedescr,
          ls_field_info TYPE rsanu_s_fieldinfo.
    FIELD-SYMBOLS:<lf_input> TYPE any.
    ASSIGN iop_value TO <lf_input>.
    IF <lf_input> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*    op_value = ip_value.
    iop_value = <lf_input>.
    lo_elem ?= cl_abap_elemdescr=>describe_by_data( <lf_input> ).
    IF NOT lo_elem->is_ddic_type( ).
      RETURN.
    ENDIF.

    DATA(ls_dfies) = lo_elem->get_ddic_field( sy-langu ).
    IF ls_dfies-convexit IS INITIAL.
      RETURN.
    ENDIF.

    IF ls_dfies-convexit = 'ALPHA'.
      iop_value = |{ <lf_input> ALPHA = IN }|.
      RETURN.
    ENDIF.
    MOVE-CORRESPONDING ls_dfies TO ls_field_info.
    TRY.
        cl_rsan_ut_conversion_exit=>try_conv_int_ext_int(
          EXPORTING
            i_fieldinfo              = ls_field_info
            i_value                  =  <lf_input>
            i_conversion_errors_type = '*'
          IMPORTING
            e_value                  =  iop_value
          EXCEPTIONS
            failed_with_message      = 1
            OTHERS                   = 2
        ).
        IF sy-subrc <> 0.
          iop_value = <lf_input>.
        ENDIF.
      CATCH cx_root.
        iop_value = <lf_input>.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD apply_conv_out.
    DATA: lo_elem       TYPE REF TO cl_abap_elemdescr,
          lo_type       TYPE REF TO cl_abap_typedescr,
          ls_field_info TYPE rsanu_s_fieldinfo.
    FIELD-SYMBOLS:<lf_input> TYPE any.
    ASSIGN ip_value TO <lf_input>.
    IF <lf_input> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*    op_value = ip_value.
    r_out = CONV string( <lf_input> ).
    lo_elem ?= cl_abap_elemdescr=>describe_by_data( <lf_input> ).
    IF NOT lo_elem->is_ddic_type( ).
      RETURN.
    ENDIF.

    DATA(ls_dfies) = lo_elem->get_ddic_field( sy-langu ).
    IF ls_dfies-convexit IS INITIAL.
      RETURN.
    ENDIF.
    IF ls_dfies-convexit = 'ALPHA'.
      r_out = |{ <lf_input> ALPHA = OUT }|.
      RETURN.
    ENDIF.
    MOVE-CORRESPONDING ls_dfies TO ls_field_info.
    TRY.
*        cl_rsan_ut_conversion_exit=>try_conv_ext_int_ext(
*          EXPORTING
*            i_fieldinfo              = CORRESPONDING #( ls_dfies )
*            i_value                  =  <lf_input>
*            i_conversion_errors_type = '*'
*          IMPORTING
*            e_value                  = r_out
*          EXCEPTIONS
*            failed_with_message      = 1
*            OTHERS                   = 2
*        ).
*        IF sy-subrc <> 0.
*          r_out = CONV #( <lf_input> ).
*        ENDIF.
        cl_rsan_ut_conversion_exit=>convert_to_extern(
          EXPORTING
            i_fieldinfo      = CORRESPONDING #( ls_dfies )
            i_internal_value = <lf_input>
          IMPORTING
            e_external_value = r_out
        ).
      CATCH cx_root.
        r_out = CONV #( <lf_input> ).
    ENDTRY.


  ENDMETHOD.


  METHOD check_plausability_date.


    CHECK ip_possible_date IS NOT INITIAL.
    CLEAR r_date.

    DATA(ld_pattern) = COND string( WHEN ip_date_pattern IS SUPPLIED THEN ip_date_pattern
                               ELSE date_pattern-sap_pattern
                             ).
    DATA: ld_year           TYPE n LENGTH 4,
          ld_month          TYPE n LENGTH 2,
          ld_day            TYPE n LENGTH 2,
          ld_formatted_date TYPE c LENGTH 8.


    CASE ld_pattern.
      WHEN date_pattern-sap_pattern OR "aaaammdd
           date_pattern-no_sep_angl_pattern OR  "mmddaaaa
           date_pattern-no_sep_no_angl_pattern. "ddmmaaaa

        IF strlen( ip_possible_date ) > 8.
          RAISE EXCEPTION TYPE zcx_ga_util
            EXPORTING
              error = |{ ip_possible_date } Incorrect date format |.
        ELSE.
          ld_formatted_date = |{ ip_possible_date ALPHA = IN }|.
        ENDIF.

      WHEN date_pattern-sep_angl_pattern OR "mm_dd_aaaa
           date_pattern-sep_no_angl_pattern."dd_mm_aaaa
        IF strlen( ip_possible_date ) <> 10.
          RAISE EXCEPTION TYPE zcx_ga_util
            EXPORTING
              error = |{ ip_possible_date } Incorrect date format |.
        ENDIF.
        ld_formatted_date = |{ ip_possible_date(2) }{ ip_possible_date+3(2) }{ ip_possible_date+6(4) }|.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_ga_util
          EXPORTING
            error = |Date pattern { ip_date_pattern } is not allowed.|.
    ENDCASE.
    IF ld_formatted_date CN '1234567890'.
      RAISE EXCEPTION TYPE zcx_ga_util
        EXPORTING
          error = |{ ip_possible_date } Incorrect date format |.
    ENDIF.
    ld_year = SWITCH #( ld_pattern
        WHEN date_pattern-sap_pattern THEN ld_formatted_date(4) "aaaammdd
        ELSE ld_formatted_date+4(4)                             "____aaaa
    ).

    ld_month = SWITCH #( ld_pattern
                WHEN date_pattern-sap_pattern            "aaaammdd
                    THEN ld_formatted_date+4(2)
                WHEN date_pattern-sep_angl_pattern OR    "Primero el mes mm_____
                     date_pattern-no_sep_angl_pattern
                    THEN ld_formatted_date(2)
                WHEN date_pattern-no_sep_no_angl_pattern OR "El mes en el medio
                     date_pattern-sep_no_angl_pattern
                    THEN ld_formatted_date+2(2)
               ).
    ld_day = SWITCH #( ld_pattern
                WHEN date_pattern-sap_pattern            "aaaammdd
                    THEN ld_formatted_date+6(2)
                WHEN date_pattern-sep_angl_pattern OR    "Primero el mes mm_____
                     date_pattern-no_sep_angl_pattern
                    THEN ld_formatted_date+2(2)
                WHEN date_pattern-no_sep_no_angl_pattern OR "Primero el día
                     date_pattern-sep_no_angl_pattern
                    THEN ld_formatted_date(2)
               ).

    IF ld_day NOT BETWEEN 1 AND 31.                      "#EC NUMBER_OK
      RAISE EXCEPTION TYPE zcx_ga_util
        EXPORTING
          error = |{ ip_possible_date } Incorrect date format. Incorrect day { ld_day }  |.
    ENDIF.
    IF ld_month NOT BETWEEN 1 AND 12.                    "#EC NUMBER_OK
      RAISE EXCEPTION TYPE zcx_ga_util
        EXPORTING
          error = |{ ip_possible_date } Incorrect date format. Incorrect Month { ld_month } |.
    ENDIF.
    r_date = |{ ld_year }{ ld_month }{ ld_day }|.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = r_date               " Traspaso de la fecha a verificar
      EXCEPTIONS
        plausibility_check_failed = 1                " Fecha no es verosímil
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      CLEAR r_date.
      RAISE EXCEPTION TYPE zcx_ga_util
        EXPORTING
          error = |{ ip_possible_date }: Not possible date { r_date }|.
    ENDIF.

    "comprobamos el año... es posible que nos vengan cosas muy raras debido a la conversion en excel
    "daremos por "raros" fechas fuera de los rango siguientes

    DATA: lr_possible_date TYPE RANGE OF sy-datum.

    lr_possible_date = VALUE #(
        ( sign = 'I' option = 'BT' low = '19000101' high = '29991231' )
        ( sign = 'I' option = 'EQ' low = '99991231' )
    ).

    op_serious_doubt = COND #( WHEN r_date NOT IN lr_possible_date THEN abap_true
                               ELSE abap_false
                             ).


  ENDMETHOD.


  METHOD check_plausability_time.
    DATA: ld_ok TYPE abap_bool.
    DATA(ld_time_string) = CONV string( ip_possible_time ).
    DATA ld_time TYPE sy-uzeit.

    op_ok = SWITCH #( ip_time_pattern
                     WHEN time_pattern-sap_pattern THEN
                        check_time_is_sap_pattern( EXPORTING ip_time_string = ld_time_string
                                          IMPORTING op_time = ld_time
                                              )
                     WHEN time_pattern-adp_pattern THEN
                        check_time_is_adp_pattern( EXPORTING ip_time_string = ld_time_string
                                          IMPORTING op_time = ld_time
                                              )
                   ).
    r_time = ld_time.

  ENDMETHOD.


  METHOD check_time_is_adp_pattern.
    CONSTANTS: lc_allowed_char TYPE string VALUE '0123456789 :APM '.
    DATA: ld_hour       TYPE n LENGTH 2,
          ld_minutes    TYPE n LENGTH 2,
          ld_seconds    TYPE n LENGTH 2,
          ld_seconds_tt TYPE c LENGTH 5,
          ld_tt         TYPE c LENGTH 2.
    DATA:      ld_time_string TYPE string.

    ld_time_string = to_upper( ip_time_string ).
    IF NOT ld_time_string CO lc_allowed_char.
      r_ok = abap_false.
      CLEAR op_time.
      RETURN.
    ENDIF.
    r_ok = abap_false.

    SPLIT ld_time_string AT ':' INTO TABLE DATA(lt_time_results).
    LOOP AT lt_time_results INTO DATA(ls_result).
      CASE sy-tabix.
        WHEN 1. "hora

          IF strlen(  ls_result ) > 2 OR NOT ls_result CO '0123456789'.
            r_ok = abap_false.
            EXIT.
          ENDIF.
          ld_hour = ls_result.
        WHEN 2. "minutos
          IF strlen(  ls_result ) > 2 OR NOT ls_result CO '0123456789'.
            r_ok = abap_false.
            EXIT.
          ENDIF.
          ld_minutes = ls_result.
        WHEN 3. "seconds + tt
          IF strlen(  ls_result ) > 5 OR NOT ls_result CO '0123456789 APM'.
            r_ok = abap_false.
            EXIT.
          ENDIF.
          ld_seconds_tt = ls_result.
          SPLIT ld_seconds_tt AT space INTO TABLE DATA(lt_sec_results).
          LOOP AT lt_sec_results INTO DATA(ls_sec_result).
            CASE sy-tabix.
              WHEN 1. "segundos
                IF strlen( ls_sec_result ) > 2 OR NOT ls_sec_result CO '0123456789'.
                  r_ok = abap_false.
                  EXIT.
                ENDIF.
                "si hemos encontrado segundos.
                "le ponemos ok
                r_ok = abap_true.
                ld_seconds = ls_sec_result.

              WHEN 2. "TT
                IF strlen( ls_sec_result ) > 2 OR NOT ls_sec_result CO 'APM'.
                  r_ok = abap_false.
                  EXIT.
                ENDIF.
                ld_tt = ls_sec_result.
            ENDCASE.
          ENDLOOP.
          IF r_ok = abap_false.
            EXIT.
          ENDIF.

      ENDCASE.
    ENDLOOP.
    IF r_ok = abap_false.
      CLEAR op_time.
      RETURN.
    ENDIF.
    r_ok = check_time_values(  CHANGING iop_hour = ld_hour
                                    iop_minutes = ld_minutes
                                    iop_seconds = ld_seconds
                                    iop_tt = ld_tt ).
    IF r_ok = abap_true.
      op_time = |{ ld_hour }{ ld_minutes }{ ld_seconds }|.
    ENDIF.


  ENDMETHOD.


  METHOD check_time_is_sap_pattern.
    IF strlen( ip_time_string ) < 6.
      r_ok = abap_false.
      RETURN.
    ENDIF.
    IF NOT ip_time_string CO '0123456789'.
      r_ok = abap_false.
      RETURN.
    ENDIF.

    DATA: ld_hour    TYPE n LENGTH 2,
          ld_minute  TYPE n LENGTH 2,
          ld_seconds TYPE n LENGTH 2.
    ld_hour = ip_time_string(2).
    ld_minute = ip_time_string+2(2).
    ld_seconds = ip_time_string+4(2).
    r_ok = check_time_values(  CHANGING iop_hour = ld_hour
                                    iop_minutes = ld_minute
                                    iop_seconds = ld_seconds ).
    IF r_ok = abap_true.
      op_time = |{ ld_hour }{ ld_minute }{ ld_seconds }|.
    ENDIF.
  ENDMETHOD.


  METHOD check_time_values.
    DATA: ld_hour    TYPE n LENGTH 2,
          ld_minutes TYPE n LENGTH 2,
          ld_seconds TYPE n LENGTH 2,
          ld_tt      TYPE c LENGTH 2.

    ld_hour = iop_hour.
    ld_minutes = iop_minutes.
    ld_seconds = iop_seconds.
    IF iop_tt IS SUPPLIED AND iop_tt IS NOT INITIAL.
      ld_tt = to_upper( iop_tt ).
      IF ld_tt <> 'PM' AND ld_tt <> 'AM'.
        CLEAR: iop_hour, iop_minutes, iop_seconds, iop_tt.
        r_ok = abap_false.
        RETURN.
      ENDIF.
    ENDIF.
    IF ( ld_hour > 24 OR ld_minutes > 59 OR ld_seconds > 59 ).
      CLEAR: iop_hour, iop_minutes, iop_seconds, iop_tt.
      r_ok = abap_false.
      RETURN.
    ENDIF.
    IF ( ld_tt = 'AM' AND ld_hour NOT BETWEEN 0 AND 11 ) OR
       ( ld_tt = 'PM' AND ld_hour NOT BETWEEN 1 AND 12 ).
      CLEAR: iop_hour, iop_minutes, iop_seconds, iop_tt.
      r_ok = abap_false.
      RETURN.
    ENDIF.
    IF ld_tt = 'PM' AND ld_hour < 12.
      ADD 12 TO ld_hour.
    ENDIF.
    IF ld_hour = 24.
      ld_hour = 00.
    ENDIF.

    r_ok = abap_true.

    iop_hour = ld_hour.
    iop_minutes = ld_minutes.
    iop_seconds = ld_seconds.



  ENDMETHOD.


  METHOD constructor.
    IF ip_logger IS BOUND.
      o_logger ?= ip_logger.
    ENDIF.
  ENDMETHOD.


  METHOD create_ref_to_range.

    DATA: lt_range TYPE RANGE OF string,
          ls_range LIKE LINE OF lt_range.
    DATA: lo_tab_desc         TYPE REF TO cl_abap_tabledescr,
          lo_str_desc         TYPE REF TO cl_abap_structdescr,
          lo_data_desc        TYPE REF TO cl_abap_elemdescr,
          lt_range_components TYPE abap_component_tab.
    CHECK ip_name_data IS NOT INITIAL OR ip_data IS SUPPLIED.

    "Definición estructura de rango
    IF ip_name_data IS NOT INITIAL.
      lo_data_desc ?= cl_abap_elemdescr=>describe_by_name( to_upper( ip_name_data ) ).
    ELSE.
      lo_data_desc ?= cl_abap_elemdescr=>describe_by_data( ip_data ).
    ENDIF.

    lo_str_desc ?= cl_abap_structdescr=>describe_by_data( ls_range ).

    lt_range_components = VALUE #(  FOR comp IN lo_str_desc->get_components(  )
                            ( name = comp-name
                              type = COND #( WHEN comp-name = 'SIGN' OR comp-name = 'OPTION' THEN comp-type
                                             ELSE lo_data_desc )
                            )
                          ).

    lo_tab_desc ?= cl_abap_tabledescr=>create( CAST cl_abap_structdescr( cl_abap_structdescr=>create( lt_range_components ) ) ).


    CREATE DATA ro_ref TYPE HANDLE lo_tab_desc.




  ENDMETHOD.


  METHOD debug.
    IF ( ip_just_batch = abap_true AND sy-batch IS INITIAL ) OR
       ( ip_user <> sy-uname ).
      RETURN.
    ENDIF.

    DATA: ld_time TYPE i.
    TRY.
        GET TIME STAMP FIELD DATA(ld_hour1).
        WHILE ld_time < ip_ms_debug_time.

          GET TIME STAMP FIELD DATA(ld_hour2).
          ld_time = ld_hour2 - ld_hour1.

        ENDWHILE.
      CATCH cx_sy_conversion_overflow.
    ENDTRY.

    RETURN.
  ENDMETHOD.


  METHOD get_components.
    DATA: lt_columns TYPE abap_component_tab,
          ls_column  TYPE abap_componentdescr,
          lo_struc   TYPE REF TO cl_abap_structdescr.

    lt_columns = io_structure->get_components( ).
    LOOP AT lt_columns INTO ls_column.
      IF ls_column-as_include = abap_true.
        lo_struc ?= ls_column-type.
*      PERFORM f_get_components USING lo_struc
*      CHANGING iot_columns.
        get_components( EXPORTING io_structure = lo_struc
                        CHANGING iot_columns = iot_columns ).
      ELSE.
        APPEND ls_column TO iot_columns.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_lvc_fcat.

    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    CASE cl_abap_datadescr=>get_data_type_kind( ip_data ).
      WHEN cl_abap_typedescr=>typekind_table.
        lo_structdescr ?= CAST cl_abap_tabledescr( cl_abap_datadescr=>describe_by_data( ip_data ) )->get_table_line_type( ).
      WHEN cl_abap_typedescr=>typekind_struct2.
        lo_structdescr ?= cl_abap_datadescr=>describe_by_data( ip_data ).
    ENDCASE.
*    DATA(lt_dfies) =  .

    rt_fcat = VALUE lvc_t_fcat( FOR ls_dfies IN cl_salv_data_descr=>read_structdescr( lo_structdescr )
    ( CORRESPONDING lvc_s_fcat( ls_dfies ) )
    ).
  ENDMETHOD.


  METHOD get_parent_class.
    CLEAR r_class.
    SELECT SINGLE refclsname INTO r_class               "#EC CI_NOORDER
    FROM seometarel
    WHERE clsname = ip_class.
  ENDMETHOD.


  METHOD get_sel_option_icon_name.
    r_icon_name = SWITCH #( ip_option
        WHEN opt-bt THEN icons_sel_option-bt
        WHEN opt-nb THEN icons_sel_option-nb
        WHEN opt-eq THEN icons_sel_option-eq
        WHEN opt-ne THEN icons_sel_option-ne
        WHEN opt-gt THEN icons_sel_option-gt
        WHEN opt-lt THEN icons_sel_option-lt
        WHEN opt-ge THEN icons_sel_option-ge
        WHEN opt-le THEN icons_sel_option-le
        WHEN opt-cp THEN icons_sel_option-cp
        WHEN opt-np THEN icons_sel_option-np
     ).
    r_icon_name = r_icon_name &&
                 SWITCH #( ip_sign
                   WHEN opt-i THEN icons_sel_option-green
                   WHEN opt-e THEN icons_sel_option-red
                 ).
  ENDMETHOD.


  METHOD get_singleton.
    r_single = m_singleton.
*    CHECK r_single IS NOT BOUND.
    IF r_single IS BOUND.
      RETURN.
    ENDIF.
    m_singleton = NEW zcl_ga_util( ip_logger ).
    r_single = m_singleton.

  ENDMETHOD.


  METHOD get_slis_fcat.
    CALL FUNCTION 'LVC_TRANSFER_TO_SLIS'
      EXPORTING
        it_fieldcat_lvc         = get_lvc_fcat( ip_data )
      IMPORTING
        et_fieldcat_alv         = rt_fcat
      EXCEPTIONS
        it_data_missing         = 1
        it_fieldcat_lvc_missing = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      CLEAR rt_fcat.
    ENDIF.

  ENDMETHOD.


  METHOD get_standard_text.
    DATA: ls_head TYPE thead.

    ls_head = VALUE #(
      tdid = ip_tdid
      tdname = ip_tdname
      tdobject = ip_tdobject

    ).

    DATA: lt_languages TYPE TABLE OF sy-langu.

    SELECT tdspras INTO TABLE lt_languages
      FROM stxh
     WHERE tdobject = ls_head-tdobject
       AND tdname = ls_head-tdname
       AND tdid   = ls_head-tdid.

    IF sy-subrc <> 0.
      CLEAR rt_text.
    ENDIF.
    "Language by parameter
    ASSIGN lt_languages[ table_line = ip_spras ] TO FIELD-SYMBOL(<lf_langu>).

    IF <lf_langu> IS NOT ASSIGNED.
      "Englis
      ASSIGN lt_languages[ table_line = 'E' ] TO <lf_langu>.
    ENDIF.

    IF <lf_langu> IS NOT ASSIGNED.
      "Spanish
      ASSIGN lt_languages[ table_line = 'S' ] TO <lf_langu>.
    ENDIF.
    IF <lf_langu> IS NOT ASSIGNED.
      "first found language
      ASSIGN lt_languages[ 1 ] TO <lf_langu>.
    ENDIF.
    IF <lf_langu> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    ls_head-tdspras = <lf_langu>.



    IF read_text(
         EXPORTING
           is_head = ls_head
       IMPORTING
         ot_text = rt_text                 " Tipo de tabla para thead
       ).
      RETURN.
    ENDIF.


  ENDMETHOD.


  METHOD icon_create.
    DATA: ld_icon_name TYPE icon-name.
    DATA: ld_info_text TYPE icont-quickinfo.
    DATA: ld_add_info  TYPE icon-internal.
    FIELD-SYMBOLS: <icon> TYPE any.

    ld_add_info = abap_true.
    CLEAR ld_info_text.
    CLEAR ld_add_info.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = ip_icon
        info                  = ld_info_text
        add_stdinf            = ld_add_info
      IMPORTING
        result                = iop_icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      CLEAR iop_icon.
    ENDIF.
    IF iop_text IS SUPPLIED.
      ld_icon_name = ip_icon.
      CALL FUNCTION 'ICON_CHECK'
        EXPORTING
          icon_name      = ld_icon_name
          language       = sy-langu
          button         = ' '
          status         = ' '
          message        = ' '
          function       = ' '
          textfield      = 'X'
          locked         = ' '
        IMPORTING
          icon_text      = ld_info_text
*         ICON_SIZE      =
*         ICON_ID        =
        EXCEPTIONS
          icon_not_found = 1
          OTHERS         = 2.

      IF sy-subrc = 0.
        iop_text = ld_info_text.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_subclass.
    r_ok = abap_false.
    DATA: ld_subclass TYPE seoclsname,
          lo_obj_desc TYPE REF TO cl_abap_objectdescr.
    IF ip_subclass IS INITIAL AND ip_subclass_obj IS NOT BOUND.
      RETURN.
    ENDIF.

    IF ip_subclass IS NOT INITIAL AND ip_subclass_obj IS NOT BOUND.
      ld_subclass = ip_subclass.
    ENDIF.
    IF ld_subclass IS INITIAL AND ip_subclass_obj IS BOUND.
      ld_subclass = cl_abap_classdescr=>get_class_name( p_object = ip_subclass_obj ).
      IF strlen( ld_subclass ) > 7.
        ld_subclass = ld_subclass+7.
      ENDIF.
    ENDIF.
    r_ok = boolc( ip_class = ld_subclass ). "aceptamos que la propia clase es padre.
    IF r_ok = abap_true.
      RETURN.
    ENDIF.

    DATA ld_parent TYPE seoclsname.
    ld_parent = get_parent_class( ld_subclass ).
*    ld_parent =
    r_ok = boolc( ip_class = ld_parent ).
    WHILE ld_parent IS NOT INITIAL AND r_ok = abap_false.
      ld_parent = get_parent_class( ld_parent ).
      r_ok = boolc( ip_class = ld_parent ).
    ENDWHILE.
  ENDMETHOD.


  METHOD log_d.

    IF o_logger IS NOT BOUND.
      RETURN.
    ENDIF.
    TRY.
*        CHECK o_logger IS BOUND.
        o_logger->debug( iv_text ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD log_e.
    IF o_logger IS NOT BOUND.
      RETURN.
    ENDIF.
    TRY.
*        CHECK o_logger IS BOUND.
        o_logger->error( iv_text ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD log_exception.
    IF o_logger IS NOT BOUND.
      RETURN.
    ENDIF.
    TRY.
*        CHECK o_logger IS BOUND.
        o_logger->exception( ix_ex ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD log_i.
    IF o_logger IS NOT BOUND.
      RETURN.
    ENDIF.
    TRY.
*        CHECK o_logger IS BOUND.
        o_logger->info( iv_text ).
      CATCH cx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD log_w.
    IF o_logger IS NOT BOUND.
      RETURN.
    ENDIF.
    TRY.
*        CHECK o_logger IS BOUND.
        o_logger->warning( iv_text ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD popup_to_confirm.
    DATA lt_param  TYPE abap_func_parmbind_tab.
    DATA ls_param  TYPE abap_func_parmbind.
    DATA lt_exc    TYPE abap_func_excpbind_tab.
    DATA ls_exc    TYPE abap_func_excpbind.
    DATA lv_answer TYPE char1.

    DEFINE add_param.
      GET REFERENCE OF &1 INTO ls_param-value.
      ls_param-name = &2.
      ls_param-kind = &3.
      INSERT ls_param INTO TABLE lt_param.
    END-OF-DEFINITION.

    " Parameters
    add_param iv_title           'TITLEBAR'               abap_func_exporting.
    add_param iv_question        'TEXT_QUESTION'          abap_func_exporting.
    add_param iv_icon_1          'ICON_BUTTON_1'          abap_func_exporting.
    add_param iv_icon_2          'ICON_BUTTON_2'          abap_func_exporting.
    add_param iv_default         'DEFAULT_BUTTON'         abap_func_exporting.
    add_param iv_display_cancel  'DISPLAY_CANCEL_BUTTON'  abap_func_exporting.
    add_param lv_answer          'ANSWER'                 abap_func_importing.

    " Yes
    IF iv_text_1 IS SUPPLIED.
      add_param iv_text_1        'TEXT_BUTTON_1'          abap_func_exporting.
    ENDIF.

    " No
    IF iv_text_2 IS SUPPLIED.
      add_param iv_text_2        'TEXT_BUTTON_2'          abap_func_exporting.
    ENDIF.

    " Exceptions
    ls_exc-name  = 'OTHERS'.
    ls_exc-value = 1.
    INSERT ls_exc INTO TABLE lt_exc.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      PARAMETER-TABLE lt_param
      EXCEPTION-TABLE lt_exc.
    CHECK sy-subrc = 0. " No ?

    CASE lv_answer.
      WHEN '0'. " No
        rv_ok = abap_false.

      WHEN '1'. " Yes
        rv_ok = abap_true.

      WHEN 'A'. " Cancel
        rv_ok = abap_undefined.
    ENDCASE.
  ENDMETHOD.


  METHOD read_text.
    CLEAR ot_text.
    r_ok = abap_true.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = is_head-tdid
        language                = is_head-tdspras
        name                    = is_head-tdname
        object                  = is_head-tdobject
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*     IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = ot_text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      CLEAR ot_text.
      r_ok = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD show_complex_sel_options.
    DATA(lf_range) = create_ref_to_range( ip_name_data = ip_element_data ).


    FIELD-SYMBOLS <lt_range>    TYPE STANDARD TABLE.
    DATA: ld_title TYPE sy-title.
    IF lf_range IS BOUND.
      ASSIGN lf_range->* TO <lt_range>.
    ENDIF.
    IF <lt_range> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*    CHECK <lt_range> IS ASSIGNED.

    <lt_range> = CONV #( iot_range ).

    ld_title = COND #( WHEN ip_title IS INITIAL THEN TEXT-q01
                       ELSE ip_title ).


    CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
      EXPORTING
        title             = ld_title
        text              = ip_text
        signed            = ip_signed
        lower_case        = ip_lower_case
        no_interval_check = ip_no_interval_check
        just_display      = ip_just_display
        just_incl         = ip_just_incl
        excluded_options  = is_excluded_options
*       description       =
*       help_field        =
*       search_help       =
*       tab_and_field     =
      TABLES
        range             = <lt_range>
      EXCEPTIONS
        no_range_tab      = 1
        cancelled         = 2
        internal_error    = 3
        invalid_fieldname = 4
        OTHERS            = 5.


    IF sy-subrc = 0 AND ip_just_display = abap_false.
      iot_range = <lt_range>.
      r_ok = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD show_messages.
    IF it_messages IS INITIAL.
      RETURN.
    ENDIF.
    DATA ld_title TYPE baltitle VALUE 'Messages'.
    IF ip_title IS NOT INITIAL.
      ld_title = ip_title.
    ENDIF.
    IF iot_answer IS SUPPLIED.
      DATA: lt_messages TYPE rs_t_msg.
      lt_messages = VALUE #( FOR ls_mess IN it_messages
                             ( CORRESPONDING #( ls_mess
                                  MAPPING msgty = type msgid = id
                                          msgno = number
                                          msgv1 = message_v1
                                          msgv2 = message_v2
                                          msgv3 = message_v3
                                          msgv4 = message_v4
                               ) )
                           ).
      CALL FUNCTION 'RSDC_SHOW_MESSAGES_POPUP'
        EXPORTING
          i_t_msg           = lt_messages
          i_txt             = ld_title
          i_with_s_on_empty = abap_false
          i_one_msg_direct  = abap_false
          i_one_msg_type_s  = abap_false
        IMPORTING
          e_s_exit_command  = iot_answer.
    ELSE.
      CALL FUNCTION 'SUSR_DISPLAY_LOG'
        EXPORTING
          display_in_popup = 'X'
          log_title        = ld_title
        TABLES
          it_log_bapiret2  = it_messages
        EXCEPTIONS
          parameter_error  = 1
          OTHERS           = 2.
    ENDIF.
  ENDMETHOD.


  METHOD show_sel_options.


    IF ip_title IS INITIAL.
      ip_title = TEXT-q01.
    ENDIF.
    IF is_optionlist IS NOT SUPPLIED.
      is_optionlist = 'XXXXXXXXXX'.
    ENDIF.
    DATA(ld_sign) = iop_sign.
    DATA(ld_option) = iop_option.
    r_ok = abap_false.
    CALL FUNCTION 'SELECT_OPTION_OPTIONS'
      EXPORTING
        selctext          = ip_title
        sign              = ld_sign
        option            = ld_option
        just_display      = ip_just_display
        signs_restriction = ip_signs_restriction
        option_list       = is_optionlist
        no_del_line       = ip_no_del_line
      IMPORTING
        sign              = ld_sign
        option            = ld_option
      EXCEPTIONS
        delete_line       = 1
        not_executed      = 2
        illegal_sign      = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    r_ok = abap_true.
    IF ip_just_display = abap_false.
      iop_sign = ld_sign.
      iop_option = ld_option.

    ENDIF.
  ENDMETHOD.
  METHOD get_rfc_table.
    DATA:
      lt_raw_tab    TYPE TABLE OF tab512,
      ls_raw_line   TYPE tab512,
      ld_field      TYPE string,
      lr_workarea   TYPE REF TO data,
      lo_line_type  TYPE REF TO cl_abap_structdescr,
      lo_table_type TYPE REF TO cl_abap_tabledescr,
      ls_field      TYPE LINE OF esh_t_co_rfcrt_fields.
    DATA: lo_table TYPE REF TO data.

*    IF ip_buffered = abap_true AND ip_connector IS INITIAL.
*      ot_message     = get_return_message(
*      iv_type   = 'E'
*      iv_number = '000'
*      iv_par1 = 'Parametros Incorrectos'
*      ).
*      RAISE EXCEPTION TYPE cx_grfn_exception
*        EXPORTING
*          status = ot_message.
*    ENDIF.
*
*    IF ip_buffered = abap_true.
*      READ TABLE t_tables INTO DATA(ls_table) WITH KEY
*        tabname = ip_tabname
*        id_gasystem = ip_connector-id_gasystem.
*      IF sy-subrc = 0.
*        op_data = ls_table-data.
*      ENDIF.
*    ENDIF.
*
    TRY.
        zcl_ga_util=>get_ref_to_struct(
          EXPORTING
            ip_structure_name = CONV string( ip_tabname )
            ip_rfc_dest       = ip_rfc_dest
            it_fields         = it_fields
            ip_table          = abap_true
          RECEIVING
            op_data           = op_data
        ).
      CATCH zcx_ga_util INTO DATA(lo_ex).
        RAISE EXCEPTION lo_ex.
    ENDTRY.
    IF op_data IS NOT BOUND.
      zcx_ga_util=>raise_text( |Not possible to create dynamic structure for { ip_tabname }| ).
    ENDIF.
    IF it_where_selection IS NOT INITIAL.
      DATA: lt_where     TYPE rsds_twhere,
            ls_where     TYPE LINE OF rsds_twhere,
            lt_aux_where TYPE esh_t_co_rfcrt_options.
      CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
        EXPORTING
          field_ranges  = it_where_selection
        IMPORTING
          where_clauses = lt_where.
      LOOP AT lt_where INTO ls_where
        WHERE tablename = ip_tabname.
        APPEND LINES OF ls_where-where_tab TO lt_aux_where.
      ENDLOOP.
      IF it_where IS NOT INITIAL.
        APPEND 'AND' TO lt_aux_where.
      ENDIF.
    ENDIF.

    APPEND LINES OF it_where TO lt_aux_where.


    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION ip_rfc_dest
      EXPORTING
        query_table = ip_tabname
      TABLES
        options     = lt_aux_where
        fields      = it_fields
        data        = lt_raw_tab
      EXCEPTIONS
        OTHERS      = 7.
    IF sy-subrc <> 0.
      zcx_ga_util=>raise_text( |Error Reading table { ip_tabname } at { ip_rfc_dest }| ).
    ENDIF.
    FIELD-SYMBOLS: <lf_field> TYPE any,
                   <lf_line>  TYPE any,
                   <lf_table> TYPE STANDARD TABLE.
    TRY.
*
        ASSIGN op_data->* TO <lf_table>.

        lo_table_type ?= cl_abap_tabledescr=>describe_by_data( <lf_table> ).
        lo_line_type ?= lo_table_type->get_table_line_type( ).

        CREATE DATA lr_workarea TYPE HANDLE lo_line_type.

        ASSIGN lr_workarea->* TO <lf_line>.
*
      CATCH cx_root.
        zcx_ga_util=>raise_text( |Error creating dynamic structure for table { ip_tabname }| ).
    ENDTRY.
*
    LOOP AT lt_raw_tab INTO ls_raw_line.
      LOOP AT it_fields INTO ls_field.
        ld_field = ls_raw_line+ls_field-offset(ls_field-length).
        ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE <lf_line> TO <lf_field>.
        IF <lf_field> IS ASSIGNED.
          MOVE ld_field TO <lf_field>.
        ENDIF.
      ENDLOOP.
      COLLECT <lf_line> INTO <lf_table>.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_ref_to_struct.
    DATA: lt_fields_struct TYPE TABLE OF x031l,
          ls_field_struct  TYPE x031l.
    DATA: lo_type              TYPE REF TO cl_abap_typedescr,
          lo_field_type        TYPE REF TO cl_abap_datadescr,
          lo_line_type         TYPE REF TO cl_abap_structdescr,
          lo_tabletype         TYPE REF TO cl_abap_tabledescr,
          lt_components_struct TYPE abap_component_tab,
          ls_component_struct  TYPE abap_componentdescr,
          lo_aux_ref_data      TYPE REF TO data.
    DATA: ld_length   TYPE i,
          ld_decimals TYPE i.
    CALL FUNCTION 'DD_GET_NAMETAB_FOR_RFC'
      DESTINATION ip_rfc_dest
      EXPORTING
        tabname   = CONV tabname( ip_structure_name )
      TABLES
        x031l_tab = lt_fields_struct
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      zcx_ga_util=>raise_text( iv_text = |Table { ip_structure_name } does not exist at { ip_rfc_dest }| ).

    ENDIF.

    LOOP AT lt_fields_struct INTO ls_field_struct WHERE depth = 0.
      CHECK ls_field_struct-dtyp IS NOT INITIAL.
      ld_length = ls_field_struct-exlength.
      ld_decimals = ls_field_struct-decimals.
      CLEAR ls_component_struct.
      FREE lo_type.
      TRY .
          CALL METHOD cl_abap_typedescr=>describe_by_name
            EXPORTING
              p_name         = ls_field_struct-rollname
            RECEIVING
              p_descr_ref    = lo_type
            EXCEPTIONS
              type_not_found = 1.
        CATCH cx_root.

      ENDTRY.

      IF lo_type IS NOT BOUND.
        CASE ls_field_struct-exid.
          WHEN 'STRING'.  lo_type  ?= cl_abap_elemdescr=>get_string( ).
          WHEN 'XSTRING'. lo_type  ?= cl_abap_elemdescr=>get_xstring( ).
          WHEN 'I'.       lo_type  ?= cl_abap_elemdescr=>get_i( ).
          WHEN 'F'.       lo_type  ?= cl_abap_elemdescr=>get_f( ).
          WHEN 'D'.       lo_type  ?= cl_abap_elemdescr=>get_d( ).
          WHEN 'T'.       lo_type  ?= cl_abap_elemdescr=>get_t(  ).
          WHEN 'C'.       lo_type  ?= cl_abap_elemdescr=>get_c( p_length = ld_length ).
          WHEN 'N'.       lo_type  ?= cl_abap_elemdescr=>get_n( p_length = ld_length ).
          WHEN 'X'.       lo_type  ?= cl_abap_elemdescr=>get_x( p_length = ld_length ).
          WHEN 'P'.
            ld_length = CONV i( ls_field_struct-dblength ).
            ld_decimals = CONV i( ls_field_struct-decimals ).
            lo_type  ?= cl_abap_elemdescr=>get_p( p_length = ld_length p_decimals = ld_decimals ).
          WHEN OTHERS.
            CASE ls_field_struct-dtyp.
              WHEN 'STR1'.
*              PERFORM f_get_ref_to_struc USING ls_field_struct-rollname
*                    ip_rfc_dest
*              CHANGING lo_aux_ref_data.
                lo_aux_ref_data = get_ref_to_struct( EXPORTING ip_structure_name = CONV string( ls_field_struct-rollname )
                                                               ip_rfc_dest = ip_rfc_dest
                                                               it_fields = it_fields ).
                lo_type ?= cl_abap_structdescr=>describe_by_data_ref( lo_aux_ref_data ).
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.
        ENDCASE.
      ENDIF.
      CHECK lo_type IS BOUND.
      CLEAR ls_component_struct.
      ls_component_struct-name = ls_field_struct-fieldname.
      ls_component_struct-type ?= lo_type.
      IF it_fields IS NOT INITIAL.
        READ TABLE it_fields TRANSPORTING NO FIELDS WITH KEY fieldname = ls_component_struct-name.
        IF sy-subrc <> 0. "si no lo encuentro en las peticiones... continuo
          CONTINUE.
        ENDIF.
      ENDIF.
      APPEND ls_component_struct TO lt_components_struct.
    ENDLOOP.
    lo_line_type = cl_abap_structdescr=>create( lt_components_struct ).
    CASE ip_table.
      WHEN abap_false.
        CREATE DATA op_data TYPE HANDLE lo_line_type.
      WHEN abap_true.
        lo_tabletype =
                  cl_abap_tabledescr=>create( p_line_type = lo_line_type ).
        CREATE DATA op_data TYPE HANDLE lo_tabletype.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
