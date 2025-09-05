class ZCL_GA_UTIL definition
  public
  create public .

public section.

  constants:
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
  constants:
    BEGIN OF date_pattern,
        sap_pattern            TYPE string VALUE 'aaaammdd',
        no_sep_no_angl_pattern TYPE string VALUE 'ddmmaaaa',
        no_sep_angl_pattern    TYPE string VALUE 'mmddaaaa',
        sep_no_angl_pattern    TYPE string VALUE 'dd+mm+aaaa',
        sep_angl_pattern       TYPE string VALUE 'mm+dd+aaaa',
      END OF date_pattern .
  constants:
    BEGIN OF time_pattern,
        sap_pattern TYPE string VALUE 'hhmmss',
        adp_pattern TYPE string VALUE 'hh:mm:ss tt',
      END OF time_pattern .

    "EXPORTING VALUE(op_value) TYPE any
  class-methods APPLY_CONV_OUT
    importing
      !IP_VALUE type ANY
    returning
      value(R_OUT) type STRING .
  class-methods APPLY_CONV         "IMPORTING VALUE(ip_value) TYPE any
    changing
      !IOP_VALUE type ANY
    raising
      ZCX_GA_UTIL .
  class-methods ICON_CREATE
    importing
      value(IP_ICON) type ANY
    changing
      value(IOP_ICON) type ANY
      value(IOP_TEXT) type ANY optional .
  class-methods GET_SEL_OPTION_ICON_NAME
    importing
      value(IP_SIGN) type ANY
      value(IP_OPTION) type ANY
    returning
      value(R_ICON_NAME) type STRING .
  class-methods SHOW_SEL_OPTIONS
    importing
      value(IP_TITLE) type SY-TITLE optional
      value(IP_JUST_DISPLAY) type ABAP_BOOL default ABAP_FALSE
      value(IS_OPTIONLIST) type RSOPTIONS optional
      value(IP_SIGNS_RESTRICTION) type RALDB-SIGN optional
      value(IP_NO_DEL_LINE) type ABAP_BOOL default ABAP_TRUE
    changing
      value(IOP_SIGN) type TVARV_SIGN optional
      value(IOP_OPTION) type TVARV_OPTI optional
    returning
      value(R_OK) type ABAP_BOOL .
  class-methods SHOW_COMPLEX_SEL_OPTIONS
    importing
      !IP_TITLE type SY-TITLE optional
      !IP_TEXT type RSSELTEXT optional
      !IP_SIGNED type ABAP_BOOL default ABAP_TRUE
      !IP_LOWER_CASE type ABAP_BOOL default ABAP_FALSE
      !IP_NO_INTERVAL_CHECK type ABAP_BOOL default ABAP_TRUE
      !IP_JUST_DISPLAY type ABAP_BOOL default ABAP_FALSE
      !IP_JUST_INCL type ABAP_BOOL default ABAP_FALSE
      !IS_EXCLUDED_OPTIONS type RSOPTIONS optional
      !IP_ELEMENT_DATA type STRING default 'STRING'
    changing
      !IOT_RANGE type ANY TABLE
    returning
      value(R_OK) type ABAP_BOOL .
  class-methods POPUP_TO_CONFIRM
    importing
      !IV_TITLE type CSEQUENCE
      !IV_QUESTION type CSEQUENCE
      !IV_ICON_1 type ICON-NAME default 'ICON_OKAY'
      !IV_TEXT_1 type CSEQUENCE optional
      !IV_ICON_2 type ICON-NAME default 'ICON_CANCEL'
      !IV_TEXT_2 type CSEQUENCE optional
      !IV_DEFAULT type CHAR1 default '2'
      !IV_DISPLAY_CANCEL type ABAP_BOOL optional
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods CREATE_REF_TO_RANGE
    importing
      !IP_NAME_DATA type STRING optional
      !IP_DATA type ANY optional
    returning
      value(RO_REF) type ref to DATA .
  class-methods DEBUG
    importing
      value(IP_MS_DEBUG_TIME) type I default 60          "#EC NUMBER_OK
      value(IP_JUST_BATCH) type ABAP_BOOL default ABAP_TRUE
      value(IP_USER) type SY-UNAME default SY-UNAME .
  class-methods GET_COMPONENTS
    importing
      !IO_STRUCTURE type ref to CL_ABAP_STRUCTDESCR
    changing
      !IOT_COLUMNS type ABAP_COMPONENT_TAB .
  methods CONSTRUCTOR
    importing
      value(IP_LOGGER) type ref to ZCL_ALOG_MSG_LOGGER_BASE optional .
  methods LOG_D
    importing
      value(IV_TEXT) type CSEQUENCE .
  methods LOG_E
    importing
      value(IV_TEXT) type CSEQUENCE .
  methods LOG_I
    importing
      value(IV_TEXT) type CSEQUENCE .
  methods LOG_W
    importing
      value(IV_TEXT) type CSEQUENCE .
  methods LOG_EXCEPTION
    importing
      value(IX_EX) type ref to CX_ROOT .
  class-methods GET_SINGLETON
    importing
      value(IP_LOGGER) type ref to ZCL_ALOG_MSG_LOGGER_BASE optional
    returning
      value(R_SINGLE) type ref to ZCL_GA_UTIL .
  class-methods GET_LVC_FCAT
    importing
      !IP_DATA type ANY
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  class-methods GET_SLIS_FCAT
    importing
      !IP_DATA type ANY
    returning
      value(RT_FCAT) type SLIS_T_FIELDCAT_ALV .
  class-methods CHECK_PLAUSABILITY_DATE
    importing
      !IP_POSSIBLE_DATE type ANY
      !IP_DATE_PATTERN type STRING optional
    exporting
      value(OP_SERIOUS_DOUBT) type ABAP_BOOL
    returning
      value(R_DATE) type SY-DATUM
    raising
      ZCX_GA_UTIL .
  class-methods CHECK_PLAUSABILITY_TIME
    importing
      !IP_POSSIBLE_TIME type ANY
      !IP_TIME_PATTERN type STRING default TIME_PATTERN-SAP_PATTERN
    exporting
      value(OP_OK) type ABAP_BOOL
    returning
      value(R_TIME) type SY-UZEIT
    raising
      ZCX_GA_UTIL .
  class-methods IS_SUBCLASS
    importing
      value(IP_CLASS) type SEOCLSNAME
      value(IP_SUBCLASS) type SEOCLSNAME optional
      value(IP_SUBCLASS_OBJ) type ref to OBJECT optional
    returning
      value(R_OK) type ABAP_BOOL .
  class-methods SHOW_MESSAGES
    importing
      value(IP_TITLE) type STRING optional
      value(IT_MESSAGES) type BAPIRET2_T optional
    changing
      value(IOT_ANSWER) type BAL_S_EXCM optional .
  class-methods READ_TEXT
    importing
      value(IS_HEAD) type THEAD
    exporting
      value(OT_TEXT) type BBPT_TLINE
    returning
      value(R_OK) type ABAP_BOOL .
  class-methods GET_STANDARD_TEXT
    importing
      value(IP_TDOBJECT) type TDOBJECT
      value(IP_TDNAME) type TDOBNAME
      value(IP_TDID) type TDID optional
      value(IP_SPRAS) type SPRAS default SY-LANGU
    returning
      value(RT_TEXT) type BBPT_TLINE .
  class-methods CHECK_TIME_IS_ADP_PATTERN
    importing
      !IP_TIME_STRING type STRING
    exporting
      !OP_TIME type T
    returning
      value(R_OK) type ABAP_BOOL .
  class-methods CHECK_TIME_IS_SAP_PATTERN
    importing
      !IP_TIME_STRING type STRING
    exporting
      !OP_TIME type T
    returning
      value(R_OK) type ABAP_BOOL .
  class-methods CHECK_TIME_VALUES
    changing
      !IOP_HOUR type N
      !IOP_MINUTES type N
      !IOP_SECONDS type N
      !IOP_TT type C optional
    returning
      value(R_OK) type ABAP_BOOL .
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
private section.

  data O_LOGGER type ref to ZCL_ALOG_MSG_LOGGER_BASE .
  class-data M_SINGLETON type ref to ZCL_GA_UTIL .

  class-methods GET_PARENT_CLASS
    importing
      value(IP_CLASS) type SEOCLSNAME
    returning
      value(R_CLASS) type SEOCLSNAME .
ENDCLASS.



CLASS ZCL_GA_UTIL IMPLEMENTATION.


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


METHOD POPUP_TO_CONFIRM.
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
ENDCLASS.
