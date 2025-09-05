class ZCL_GA_RETROFIT definition
  public
  inheriting from ZCL_GA_UTIL
  final
  create public .

public section.

  types:
    BEGIN OF ty_s_system,
      sysid  TYPE sysid,
      mandt  TYPE mandt,
      rfc    TYPE rfcdest,
      folder TYPE string,
    END OF ty_s_system .

  methods ADD_TO_BUFFER
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_RETROFIT .
  methods COPY_FILES
    importing
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_RETROFIT .
  methods DESTIN_FILES_EXIST
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_RETROFIT .
  methods IMPORT
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_RETROFIT .
  methods CONSTRUCTOR
    importing
      value(IP_ORDER) type TRKORR
      value(IP_LOGGER) type ref to ZCL_ALOG_MSG_LOGGER_BASE optional
      value(IP_SOURCE_SYSTEM) type SYSID
      value(IP_DESTIN_SYSTEM) type SYSID
    raising
      ZCX_GA_RETROFIT .
  methods GET_COFILE
    returning
      value(R_SYSTEM) type STRING .
  methods GET_DATAFILE
    returning
      value(R_SYSTEM) type STRING .
  methods GET_DESTIN_SYSTEM
    returning
      value(R_SYSTEM) type TY_S_SYSTEM .
  methods GET_SOURCE_SYSTEM
    returning
      value(R_SYSTEM) type TY_S_SYSTEM .
protected section.
PRIVATE SECTION.

  DATA m_cofile TYPE string .
  DATA m_datafile TYPE string .
  CONSTANTS:
    BEGIN OF c_tst_system,
      sysid  TYPE sysid VALUE 'TST',
      mandt  TYPE mandt VALUE '010',
      rfc    TYPE rfcdest VALUE 'NONE',
      folder TYPE string VALUE '\\sapazssi\pub-saptrans\R3\',
    END OF c_tst_system .
  CONSTANTS:
    BEGIN OF c_hrd_system,
      sysid  TYPE sysid VALUE 'HRD',
      mandt  TYPE mandt VALUE '020',
      rfc    TYPE rfcdest VALUE 'HRD',
      folder TYPE string VALUE '\\sapazssi\pub-saptrans\HR\',
    END OF c_hrd_system .
  CONSTANTS:
    BEGIN OF c_ind_system,
      sysid  TYPE sysid VALUE 'IND',
      mandt  TYPE mandt VALUE '100',
      rfc    TYPE rfcdest VALUE 'IND',
      folder TYPE string VALUE '\\MAZVMIND01\trans\',
    END OF c_ind_system .
  CONSTANTS:
    BEGIN OF c_grd_system,
      sysid  TYPE sysid VALUE 'GRD',
      mandt  TYPE mandt VALUE '001',
      rfc    TYPE rfcdest VALUE 'GRD',
      folder TYPE string VALUE '\\GANVMGRD01\sapmnt\trans\',
    END OF c_grd_system .
  CONSTANTS:
    BEGIN OF c_par_system,
      sysid  TYPE sysid VALUE 'PAR',
      mandt  TYPE mandt VALUE '010',
      rfc    TYPE rfcdest VALUE 'PAR',
      folder TYPE string VALUE '\\GANVMQGA22\sapmnt\trans\',
    END OF c_par_system .
  CONSTANTS:
    BEGIN OF c_lcl_system,
      sysid TYPE sysid VALUE 'LCL',
      mandt TYPE mandt VALUE space,
      rfc   TYPE rfcdest VALUE space,
*      folder TYPE string VALUE ,
    END OF c_lcl_system .
  DATA m_order TYPE trkorr .
  DATA m_source_system TYPE ty_s_system .
  DATA m_destin_system TYPE ty_s_system .

  METHODS execute_command
    IMPORTING
      VALUE(ip_command)  TYPE csequence
      VALUE(ip_params)   TYPE csequence OPTIONAL
      VALUE(ip_rfc_dest) TYPE csequence DEFAULT 'NONE'
    EXPORTING
      VALUE(op_status)   TYPE btcxpgstat
      VALUE(op_exitcode) TYPE btcxpgexit
      VALUE(ot_protocol) TYPE lca_tracefile_tab
    RETURNING
      VALUE(r_bool)      TYPE abap_bool
    RAISING
      zcx_ga_retrofit .
ENDCLASS.



CLASS ZCL_GA_RETROFIT IMPLEMENTATION.


  METHOD add_to_buffer.
    DATA: ld_stat      TYPE btcxpgstat,
          ld_exit_code TYPE btcxpgexit,
          lt_protocol  TYPE lca_tracefile_tab.
    TRY.
        r_bool = execute_command(
          EXPORTING
            ip_command = 'ZBUFFER'
            ip_params  = m_order
            ip_rfc_dest = m_destin_system-rfc
          IMPORTING
           op_status  = ld_stat
           op_exitcode = ld_exit_code
           ot_protocol = lt_protocol
          ).
        CHECK r_bool = abap_true.
      CATCH zcx_ga_retrofit INTO DATA(lx_error).
        log_exception( lx_error ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ip_logger ).



    DATA: ld_system TYPE string.
    IF ip_source_system IS NOT INITIAL.
      ld_system = |C_{ ip_source_system }_SYSTEM|.
      ASSIGN (ld_system)  TO FIELD-SYMBOL(<lf_system>).
      IF <lf_system> IS NOT ASSIGNED.
*        zcx_ga_retrofit=>raise_text( |System { ip_source_system } is not defined| ).
        RAISE EXCEPTION TYPE zcx_ga_retrofit
          EXPORTING
            error = CONV #( |System { ip_source_system } is not defined| ).
      ENDIF.
      m_source_system = <lf_system>.
    ENDIF.


    IF ip_destin_system IS NOT INITIAL.
      UNASSIGN <lf_system>.
      ld_system = |C_{ ip_destin_system }_SYSTEM|.
      ASSIGN (ld_system)  TO <lf_system>.
      IF <lf_system> IS NOT ASSIGNED.
*        zcx_ga_retrofit=>raise_text( |System { ip_destin_system } is not defined| ).
        RAISE EXCEPTION TYPE zcx_ga_retrofit
          EXPORTING
            error = CONV #( |System { ip_destin_system } IS NOT defined| ).
      ENDIF.
      m_destin_system = <lf_system>.
    ENDIF.



    m_order = ip_order.
    m_cofile = |{ m_order+3 }.{ m_order(3) }|.
    m_datafile = |R{ m_cofile+1 }|.
    TRY.
        IF m_source_system IS NOT INITIAL.
          IF NOT zcl_ga_file=>file_exists( ip_file = |{ m_source_system-folder }\\cofiles\\{ m_cofile } | ip_local = abap_false ).
*            zcx_ga_retrofit=>raise_text( |Cofile { m_cofile } does not exist at source| ).
            RAISE EXCEPTION TYPE zcx_ga_retrofit
              EXPORTING
                error = CONV #( |Cofile { m_cofile } does not exist at source| ).
          ENDIF.

          IF NOT zcl_ga_file=>file_exists( ip_file = |{ m_source_system-folder }\\data\\{ m_datafile } | ip_local = abap_false ).
*            zcx_ga_retrofit=>raise_text( |Datafile { m_datafile } does not exist at source| ).
            RAISE EXCEPTION TYPE zcx_ga_retrofit
              EXPORTING
                error = CONV #( |Cofile { m_datafile } does NOT exist AT source| ).
          ENDIF.
        ENDIF.
        IF m_destin_system IS NOT INITIAL.
          DATA: ld_folder TYPE string.
          ld_folder = |{ m_destin_system-folder }\\cofiles\\|.
          IF NOT zcl_ga_file=>folder_exists( EXPORTING  ip_local = abap_false CHANGING ip_folder =  ld_folder ).
*            zcx_ga_retrofit=>raise_text( |No access to Cofile Folder at { m_destin_system-sysid }|  ).
            RAISE EXCEPTION TYPE zcx_ga_retrofit
              EXPORTING
                error = CONV #( |No access to Cofile Folder at { m_destin_system-sysid }| ).
          ENDIF.
          ld_folder = |{ m_destin_system-folder }\\data\\|.
          IF NOT zcl_ga_file=>folder_exists( EXPORTING  ip_local = abap_false CHANGING ip_folder =  ld_folder ).
*            zcx_ga_retrofit=>raise_text( |No access to Data Folder at { m_destin_system-sysid }| ).
            RAISE EXCEPTION TYPE zcx_ga_retrofit
              EXPORTING
                error = CONV #( |No access to Data Folder at { m_destin_system-sysid }| ).
          ENDIF.
        ENDIF.

      CATCH zcx_ga_file INTO DATA(lx_file).
*        zcx_ga_retrofit=>raise_text( lx_file->get_text( ) ).
        RAISE EXCEPTION TYPE zcx_ga_retrofit
          EXPORTING
            error = CONV #( lx_file->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD copy_files.
    r_bool = abap_false.
    TRY.
        "cofile
        CASE zcl_ga_file=>copy_file(
          EXPORTING
            ip_source      = |{ m_source_system-folder }\\cofiles\\{ m_cofile }|
            ip_source_local = abap_false
            ip_destination = |{ m_destin_system-folder }\\cofiles\\{ m_cofile }|
            ip_destination_local = abap_false
            ip_overwrite   = ip_overwrite
        ).
          WHEN abap_true.
            log_i( |Fichero cofile { m_cofile } copiado | ).
            "datafiles
            CASE zcl_ga_file=>copy_file(
                        EXPORTING
                          ip_source      = |{ m_source_system-folder }\\data\\{ m_datafile }|
                          ip_source_local = abap_false
                          ip_destination = |{ m_destin_system-folder }\\data\\{ m_datafile }|
                          ip_destination_local = abap_false
                          ip_overwrite   = ip_overwrite
                  ).
              WHEN abap_true.
                log_i( |Fichero dafile { m_datafile } copiado | ).
                r_bool = abap_true.
              WHEN abap_false.
                log_e( |Error al copiar fichero datafile { m_datafile }| ).
                r_bool = abap_false.
            ENDCASE.
          WHEN abap_false.
            log_e( |Error al copiar fichero cofile { m_cofile }| ).
            r_bool = abap_false.

        ENDCASE.


      CATCH zcx_ga_file INTO DATA(lx_file).
        log_exception( lx_file ).
*        zcx_ga_retrofit=>raise_text( lx_file->get_text( ) ).
        RAISE EXCEPTION TYPE zcx_ga_retrofit
          EXPORTING
            error = CONV #( lx_file->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD destin_files_exist.

    TRY.
        r_bool = zcl_ga_file=>file_exists( ip_file = |{ m_destin_system-folder }\\cofiles\\{ m_cofile } | ip_local = abap_false ).
        CHECK r_bool = abap_false.
        r_bool = zcl_ga_file=>file_exists( ip_file = |{ m_destin_system-folder }\\data\\{ m_datafile } | ip_local = abap_false ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        log_exception( lx_error ).
*        zcx_ga_retrofit=>raise_text( lx_error->get_text( ) ).
        RAISE EXCEPTION TYPE zcx_ga_retrofit
          EXPORTING
            error = CONV #( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


METHOD execute_command.
    DATA: ld_command TYPE sxpglogcmd,
          ld_params  TYPE btcxpgpar.



    ld_command = ip_command.
    ld_params = ip_params.
    log_i( |Ejecutando { ld_command } { ld_params } en { ip_rfc_dest }| ).
    CALL FUNCTION 'SXPG_CALL_SYSTEM'
      DESTINATION ip_rfc_dest
      EXPORTING
        commandname                = ld_command
        additional_parameters      = ld_params
*       TRACE                      =
      IMPORTING
        status                     = op_status
        exitcode                   = op_exitcode
      TABLES
        exec_protocol              = ot_protocol
      EXCEPTIONS
        no_permission              = 1
        command_not_found          = 2
        parameters_too_long        = 3
        security_risk              = 4
        wrong_check_call_interface = 5
        program_start_error        = 6
        program_termination_error  = 7
        x_error                    = 8
        parameter_expected         = 9
        too_many_parameters        = 10
        illegal_command            = 11
        OTHERS                     = 12.
    IF sy-subrc <> 0.
      zcx_ga_retrofit=>raise_symsg( ).
    ENDIF.
    CASE op_status.
      WHEN 'O'.
        r_bool = abap_true.
        log_i( |Comando ejecutado| ).
      WHEN OTHERS.
        r_bool = abap_false.
        log_e( |Comando no ejecutado| ).
        LOOP AT ot_protocol INTO DATA(ls_protocol).
          log_e( ls_protocol-message ).
        ENDLOOP.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_COFILE.
    r_system = m_cofile.
  ENDMETHOD.


  METHOD get_datafile.
    r_system = m_datafile.
  ENDMETHOD.


  METHOD get_destin_system.
    r_system = m_destin_system.
  ENDMETHOD.


  METHOD GET_SOURCE_SYSTEM.
    r_system = m_source_system.
  ENDMETHOD.


  METHOD import.
    DATA: ld_stat      TYPE btcxpgstat,
          ld_exit_code TYPE btcxpgexit,
          lt_protocol  TYPE lca_tracefile_tab.
    TRY.
        r_bool = execute_command(
        EXPORTING
          ip_command = 'ZIMPORT'
          ip_params  = |{ m_order } { m_destin_system-mandt }|
          ip_rfc_dest = m_destin_system-rfc
        IMPORTING
          op_status  = ld_stat
          op_exitcode = ld_exit_code
          ot_protocol = lt_protocol
          ).
        CHECK r_bool = abap_true.
      CATCH zcx_ga_retrofit INTO DATA(lx_error).
        log_exception( lx_error ).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
