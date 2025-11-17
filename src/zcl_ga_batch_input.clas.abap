class ZCL_GA_BATCH_INPUT definition
  public
  create public .

*"* public components of class ZCL_GA_BATCH_INPUT
*"* do not include other source files here!!!
public section.

  types:
    BEGIN OF ty_s_options_session,
                 group    TYPE apqi-groupid,
                 user     TYPE apqi-userid,
                 keep     TYPE apqi-qerase,
                 holddate TYPE apqi-startdate,
               END OF ty_s_options_session .

  methods CLOSE_SESSION
    returning
      value(RS_SESSION) type TY_S_OPTIONS_SESSION .
  methods SET_TRANSACTION
    importing
      value(IP_TCODE) type SY-TCODE
    returning
      value(R_ME) type ref to ZCL_GA_BATCH_INPUT
    raising
      ZCX_GA_UTIL .
  methods SET_DYNPRO
    importing
      value(IP_REPID) type SY-REPID
      value(IP_DYNNR) type SY-DYNNR
    returning
      value(R_ME) type ref to ZCL_GA_BATCH_INPUT .
  methods SET_OKCODE
    importing
      value(OKCODE) type STRING
    returning
      value(R_ME) type ref to ZCL_GA_BATCH_INPUT .
  methods SET_FIELD
    importing
      value(IP_FIELD) type CSEQUENCE
      value(IP_VALUE) type ANY
      value(IP_DO_NOT_CONVERT) type ABAP_BOOL default ABAP_FALSE
      value(IP_REF_VALUE) type ANY optional
    returning
      value(R_ME) type ref to ZCL_GA_BATCH_INPUT .
  methods CALL_TRANSACTION
    importing
      value(ITRANS) type TCODE
      value(IMODE) type CHAR1 default 'A'
      value(UPDATE) type CHAR1 default 'A'
      value(OPTIONS) type CTU_PARAMS optional
    changing
      value(MESSAGES) type TAB_BDCMSGCOLL optional
    returning
      value(R_OK) type ABAP_BOOL .
  methods GET_MESSAGES
    returning
      value(MESSAGES) type TAB_BDCMSGCOLL .
  methods GET_BAPIRET2
    returning
      value(R_RETURN) type TT_BAPIRET2 .
  methods SET_SESSION
    importing
      value(IP_SESSION) type ABAP_BOOL default ABAP_TRUE
      value(IS_OPTIONS) type TY_S_OPTIONS_SESSION optional
    returning
      value(R_ME) type ref to ZCL_GA_BATCH_INPUT
    raising
      ZCX_GA_UTIL .
protected section.
*"* protected components of class ZCL_GA_BATCH_INPUT
*"* do not include other source files here!!!

  data:
    mt_bdcdata TYPE TABLE OF bdcdata .
  data MT_MESSAGES type TAB_BDCMSGCOLL .
  data:
    mt_return   type table of bapiret2 .
  data MD_SESSION type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data MS_OPTIONS_SESSION type TY_S_OPTIONS_SESSION .

  methods INSERT_BDCDATA
    importing
      value(IP_TCODE) type CSEQUENCE .
  methods OPEN_GROUP .
private section.
*"* private components of class ZCL_GA_BATCH_INPUT
*"* do not include other source files here!!!

  data MD_SESSION_OPENED type ABAP_BOOL .

  methods IS_CUKY
    importing
      value(IP_WAERS) type CSEQUENCE
    returning
      value(R_OK) type ABAP_BOOL .
  methods ADD_BDCDATA
    importing
      value(IS_BDCDATA) type BDCDATA .
  methods GET_VALUE
    importing
      value(IP_VALUE) type ANY
      value(IP_REF_VALUE) type ANY optional
    returning
      value(R_VALUE) type BDCDATA-FVAL .
ENDCLASS.



CLASS ZCL_GA_BATCH_INPUT IMPLEMENTATION.


  method ADD_BDCDATA.
    append is_bdcdata to mt_bdcdata.
  endmethod.


METHOD call_transaction.


  TRY.
      options-dismode = imode.
      options-updmode = update.
      CALL TRANSACTION itrans USING mt_bdcdata
                              OPTIONS FROM options
                              MESSAGES INTO me->mt_messages.
    CATCH cx_sy_authorization_error.
      r_ok = abap_false.
      RETURN.
  ENDTRY.
  REFRESH mt_bdcdata.
  r_ok = abap_true.
  IF messages IS SUPPLIED.
    messages = me->mt_messages.
  ENDIF.
ENDMETHOD.


  METHOD close_session.
    rs_session = ms_options_session.
    IF md_session = abap_true.
      CALL FUNCTION 'BDC_CLOSE_GROUP'.
      CLEAR md_session_opened.
      CLEAR md_session.
      CLEAR ms_options_session.
    ENDIF.
  ENDMETHOD.


  METHOD get_bapiret2.
    IF lines( mt_return ) <> lines( mt_messages ).

      CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
        TABLES
          imt_bdcmsgcoll = mt_messages
          ext_return     = mt_return.
    ENDIF.
    r_return = mt_return.
  ENDMETHOD.


  METHOD get_messages.
    messages = me->mt_messages.
  ENDMETHOD.


  METHOD get_value.
    DATA(ld_type_kind) = cl_abap_datadescr=>get_data_type_kind( ip_value ).
    CASE ld_type_kind.
      WHEN cl_abap_typedescr=>typekind_date. "fecha
        DATA: ld_date TYPE sy-datum.
        ld_date = ip_value.
        r_value = |{ ld_date DATE = USER }|.
        RETURN.
      WHEN cl_abap_typedescr=>typekind_time. "hora
        DATA: ld_time TYPE sy-uzeit.
        ld_time = ip_value.
        r_value = |{ ld_time TIME = USER }|.
        RETURN.

      WHEN OTHERS.
        DATA lo_elem TYPE REF TO cl_abap_elemdescr.
        lo_elem ?= cl_abap_elemdescr=>describe_by_data( ip_value ).
        IF lo_elem IS NOT BOUND.
          "no sabemos tipo elemento de datos, por lo que no sabemos
          "posible exit de conversion
          CASE ld_type_kind.
              "tipo caracter, es sencillo
            WHEN cl_abap_typedescr=>typekind_char OR
               cl_abap_typedescr=>typekind_csequence OR
               cl_abap_typedescr=>typekind_string. "tipo caracter

              r_value = CONV #( |{ ip_value ALPHA = OUT }| ).
              RETURN.
            WHEN cl_abap_typedescr=>typekind_decfloat OR
               cl_abap_typedescr=>typekind_decfloat16 OR
               cl_abap_typedescr=>typekind_decfloat34 OR
               cl_abap_typedescr=>typekind_float OR
               cl_abap_typedescr=>typekind_int OR
               cl_abap_typedescr=>typekind_int1 OR
               cl_abap_typedescr=>typekind_int8 OR
               cl_abap_typedescr=>typekind_int2 OR
               cl_abap_typedescr=>typekind_num OR
               cl_abap_typedescr=>typekind_numeric OR
               cl_abap_typedescr=>typekind_packed. "numero
              r_value = |{ CONV f( ip_value ) }|.
              RETURN.
            WHEN OTHERS. "no se me ocurre.
              r_value = CONV #( ip_value ).
              RETURN.
          ENDCASE.
        ELSE.
          DATA ls_dfies TYPE dfies.
          lo_elem->get_ddic_field(
            RECEIVING
              p_flddescr   = ls_dfies
            EXCEPTIONS
              not_found    = 1
              no_ddic_type = 2
              OTHERS       = 3
          ).
          IF sy-subrc <> 0.
            r_value = CONV #( ip_value ).
            RETURN.
          ENDIF.
          "si es moneda y tenemos referencia
          IF ls_dfies-datatype = 'CURR' AND ip_ref_value IS NOT INITIAL AND is_cuky( ip_ref_value ).
            DATA ld_curr TYPE f."campo tipo currency de mayor valor
            ld_curr = CONV #( ip_value ).
            r_value = |{ ld_curr CURRENCY = ip_ref_value number = User }|.
            RETURN.
          ENDIF.
*          if ls_dfies-convexit is not initial.
**          r_value = |{ CONV f( ip_value ) }|.
**          RETURN.
*            case l
*          endif.
          CASE ls_dfies-convexit.
            WHEN 'ALPHA'.
              r_value = |{ ip_value ALPHA = OUT }|.
              RETURN.
            WHEN space.
              CASE ld_type_kind.
                WHEN cl_abap_typedescr=>typekind_decfloat OR
                               cl_abap_typedescr=>typekind_decfloat16 OR
                               cl_abap_typedescr=>typekind_decfloat34 OR
                               cl_abap_typedescr=>typekind_float OR
                               cl_abap_typedescr=>typekind_int OR
                               cl_abap_typedescr=>typekind_int1 OR
                               cl_abap_typedescr=>typekind_int8 OR
                               cl_abap_typedescr=>typekind_int2 OR
                               cl_abap_typedescr=>typekind_num OR
                               cl_abap_typedescr=>typekind_numeric OR
                               cl_abap_typedescr=>typekind_packed. "numero
                  r_value = |{ CONV f( ip_value ) }|.
                WHEN OTHERS. "no se me ocurre otra opción.
                  r_value = CONV #( ip_value ).
              ENDCASE.
              RETURN.
            WHEN OTHERS. "hay exit de conversión distinta de ALPHA.

              cl_rsan_ut_conversion_exit=>try_conv_ext_int_ext(
                EXPORTING
                  i_fieldinfo              = CORRESPONDING rsanu_s_fieldinfo( ls_dfies )
                  i_value                  = ip_value
                  i_conversion_errors_type =  '*'
                IMPORTING
                  e_value                  = r_value
                EXCEPTIONS
                  failed_with_message      = 1
                  OTHERS                   = 2
              ).
              IF sy-subrc <> 0.
                r_value = CONV #( ip_value ).
                RETURN.
              ENDIF.
          ENDCASE.
        ENDIF.
    ENDCASE.
    "si hemos llegado aqui, algun caso que se ha escapado
    r_value = CONV #( ip_value ).
  ENDMETHOD.


  METHOD insert_bdcdata.
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode     = ip_tcode
      TABLES
        dynprotab = mt_bdcdata.

  ENDMETHOD.


  METHOD is_cuky.
    DATA ld_waers TYPE tcurc-waers.
    r_ok = abap_false.
    SELECT SINGLE waers INTO ld_waers
      FROM tcurc
     WHERE waers = ip_waers.
    IF sy-subrc = 0.
      r_ok = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD open_group.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client              = sy-mandt
        group               = ms_options_session-group
        user                = ms_options_session-user
        keep                = ms_options_session-keep
        holddate            = ms_options_session-holddate
      EXCEPTIONS
        client_invalid      = 1
        destination_invalid = 2
        group_invalid       = 3
        group_is_locked     = 4
        holddate_invalid    = 5
        internal_error      = 6
        queue_error         = 7
        running             = 8
        system_lock_error   = 9
        user_invalid        = 10
        OTHERS              = 11.
    IF sy-subrc <> 0.
      zcx_ga_util=>raise_text( | Error at Open Group sy-subrc { sy-subrc }| ).
    ENDIF.
    IF sy-subrc = 0.
      md_session_opened = abap_true.
    ENDIF.

  ENDMETHOD.


METHOD set_dynpro.

*  APPEND VALUE #( program = program dynpro = dynpro dynbegin = 'X' ) TO bdcdata.
  add_bdcdata( value #( program = conv #( ip_repid )
                        dynpro = conv #( ip_dynnr )
                        dynbegin = 'X' ) ).
  r_me = me.
ENDMETHOD.


METHOD set_field.
*  APPEND VALUE #( fnam = CONV #( to_upper( fnam ) ) fval = CONV #( fval ) ) TO bdcdata.
  DATA: ls_bdcdata TYPE bdcdata.
  ls_bdcdata-fnam = CONV #( to_upper( ip_field ) ).
  ls_bdcdata-fval = COND #( WHEN ip_do_not_convert = abap_true THEN CONV #( ip_value )
                            ELSE get_value( ip_value = ip_value ip_ref_value = ip_ref_value )
                          ) .
  CONDENSE ls_bdcdata-fval.
  add_bdcdata( ls_bdcdata ).
  r_me = me.
ENDMETHOD.


METHOD set_okcode.


  r_me = set_field( EXPORTING ip_field = 'BDC_OKCODE'
                              ip_value = conv #( okcode )
  ).


ENDMETHOD.


  METHOD set_session.
    IF ip_session = abap_true AND is_options IS INITIAL.
      zcx_ga_util=>raise_text( iv_text = 'Set_Session Wrong Parameters' ).
    ENDIF.
    IF ip_session = abap_false.
      clear md_session_opened.
      CLEAR ms_options_session.
      RETURN.
    ENDIF.
    IF is_options-group IS INITIAL.
      zcx_ga_util=>raise_text( iv_text = 'Session Name is required' ).
    ENDIF.
    md_session = ip_session.
    ms_options_session =
    VALUE #( group = is_options-group
             user = COND #( WHEN is_options-user IS NOT INITIAL THEN is_options-user
                            ELSE sy-uname
                           )
             keep = is_options-keep
             holddate = is_options-holddate
           ).

    open_group( ).
    r_me = me.
  ENDMETHOD.


  METHOD set_transaction.
    IF md_session_opened = abap_false.
      zcx_ga_util=>raise_text( | Session no abierta| ).
*      CATCH zcx_ga_util. " Excepción genérica GA
    ENDIF.
    IF md_session = abap_true.
      insert_bdcdata( ip_tcode ).
      REFRESH mt_bdcdata.
      r_me = me.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
