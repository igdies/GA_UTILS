class ZCL_GA_FILE definition
  public
  inheriting from ZCL_GA_UTIL
  create public .

public section.

  types:
    BEGIN OF ty_s_file_path,
        drive     TYPE pcfile-drive,
        extension TYPE string,
        name      TYPE string,
        name_ext  TYPE string,
        path      TYPE string,
      END OF ty_s_file_path .
  types:
    BEGIN OF ty_s_file,
        path      TYPE ty_s_file_path,
        xstring   TYPE xstring,
        t_rawdata TYPE solix_tab,
        bytecount TYPE i,
      END OF ty_s_file .

  class-methods COPY_FILE
    importing
      value(IP_DESTINATION) type CSEQUENCE
      value(IP_DESTINATION_LOCAL) type ABAP_BOOL default ABAP_TRUE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IP_SOURCE) type CSEQUENCE
      value(IP_SOURCE_LOCAL) type ABAP_BOOL default ABAP_TRUE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods DELETE_FILE
    importing
      value(IP_FILE) type CSEQUENCE
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods SPLIT_FILENAME
    importing
      value(IP_FILE_PATH) type CSEQUENCE
    changing
      value(IOP_PATH) type TY_S_FILE_PATH
    raising
      ZCX_GA_FILE .
  class-methods F4_LOCAL_FOLDER
    importing
      value(IP_WINDOW_TITLE) type CSEQUENCE optional
      value(IP_INITIAL_FOLDER) type CSEQUENCE optional
    returning
      value(R_LOCAL_FOLDER) type STRING
    raising
      ZCX_GA_FILE .
  class-methods F4_LOCAL_FILE
    raising
      ZCX_GA_FILE .
  class-methods FILE_EXISTS
    importing
      value(IP_FILE) type CSEQUENCE
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
    returning
      value(R_EXISTS) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods FOLDER_EXISTS
    importing
      value(IP_FOLDER) type CSEQUENCE
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
    returning
      value(R_EXISTS) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods UPLOAD_FILE
    importing
      value(IP_FILE_PATH) type CSEQUENCE
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
    returning
      value(R_FILE) type TY_S_FILE
    raising
      ZCX_GA_FILE .
  class-methods DOWNLOAD_FILE
    importing
      value(IP_FILE_PATH) type CSEQUENCE
      value(IP_LOCAL) type ABAP_BOOL default ABAP_FALSE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IP_FILE) type TY_S_FILE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods READ_CSV
    importing
      !IP_FILE type STRING
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
      value(IP_HAS_HEADER) type ABAP_BOOL default ABAP_FALSE
      value(IP_ENDOFLINE) type CSEQUENCE default ZCL_WD_CSV=>C_ENDOFLINE_CR_LF
      value(IP_DELIMITER) type CHAR1 default '"'
      value(IP_SEPARATOR) type CHAR1 default CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
      value(IP_ENCODING) type ABAP_ENCOD default '4110'
      value(IP_REPLACEMENT) type ABAP_REPL default '#'
      value(IP_CONV_EXIT) type ABAP_BOOL default ABAP_FALSE
      value(IP_TRIM_SPACES) type ABAP_BOOL default ABAP_FALSE
      value(IP_IGNORE_CERR) type ABAP_BOOL default ABAP_TRUE
    changing
      !IOT_TARGET_TABLE type STANDARD TABLE
    raising
      ZCX_GA_FILE .
  class-methods READ_XLSX
    importing
      !IP_FILE type STRING
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
      value(IP_HAS_HEADER) type ABAP_BOOL default ABAP_FALSE
      value(IP_START_ROW) type INT1 default 2
      value(IP_START_COLUMN) type INT1 default 0
    changing
      !IOT_TARGET_TABLE type STANDARD TABLE
    raising
      ZCX_GA_FILE .
  class-methods WRITE_XLSX
    importing
      !IP_FILE type STRING
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IP_HAS_HEADER) type ABAP_BOOL default ABAP_FALSE
      value(IP_START_ROW) type INT1 default 2
      value(IP_START_COLUMN) type INT1 default 0
      value(IT_SOURCE_TABLE) type STANDARD TABLE
    raising
      ZCX_GA_FILE .
  class-methods WRITE_CSV
    importing
      !IP_FILE type STRING
      value(IP_LOCAL) type ABAP_BOOL default ABAP_TRUE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IP_WITH_HEADER) type ABAP_BOOL default ABAP_FALSE
      value(IP_ENDOFLINE) type CSEQUENCE default ZCL_WD_CSV=>C_ENDOFLINE_CR_LF
      value(IP_DELIMITER) type CHAR1 default '"'
      value(IP_SEPARATOR) type CHAR1 default CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
      value(IP_ENCODING) type ABAP_ENCOD default '4110'
      value(IP_REPLACEMENT) type ABAP_REPL default '#'
      value(IP_CONV_EXIT) type ABAP_BOOL default ABAP_FALSE
      value(IP_TRIM_SPACES) type ABAP_BOOL default ABAP_FALSE
      value(IP_IGNORE_CERR) type ABAP_BOOL default ABAP_TRUE
      !IT_TARGET_TABLE type STANDARD TABLE
    raising
      ZCX_GA_FILE .
protected section.
private section.

  types:
    ty_t_xtab TYPE TABLE OF x .

  class-methods OVERWRITE_FILE
    importing
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IP_FILE) type CSEQUENCE
      value(IP_LOCAL) type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods CONVERT_X_TAB
    importing
      value(IT_TAB) type TY_T_XTAB
    changing
      value(IOP_FILE) type TY_S_FILE .
  class-methods FORMAT_FOLDER
    changing
      value(IOP_FOLDER) type CSEQUENCE .
  class-methods DELETE_FILE_AT_LOCAL
    importing
      value(IP_FILE) type CSEQUENCE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods DELETE_FILE_AT_SERVER
    importing
      value(IP_FILE) type CSEQUENCE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods COPY_FROM_SERVER_TO_SERVER
    importing
      value(IP_SOURCE) type CSEQUENCE
      value(IP_DESTINATION) type CSEQUENCE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods COPY_FROM_SERVER_TO_LOCAL
    importing
      value(IP_SOURCE) type CSEQUENCE
      value(IP_DESTINATION) type CSEQUENCE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods COPY_FROM_LOCAL_TO_SERVER
    importing
      value(IP_SOURCE) type CSEQUENCE
      value(IP_DESTINATION) type CSEQUENCE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods COPY_FROM_LOCAL_TO_LOCAL
    importing
      value(IP_SOURCE) type CSEQUENCE
      value(IP_DESTINATION) type CSEQUENCE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods LOCAL_FILE_EXISTS
    importing
      value(IP_FILE) type CSEQUENCE
    returning
      value(R_EXISTS) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods SERVER_FILE_EXISTS
    importing
      value(IP_FILE) type CSEQUENCE
    returning
      value(R_EXISTS) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods LOCAL_FOLDER_EXISTS
    importing
      value(IP_FOLDER) type CSEQUENCE
    returning
      value(R_EXISTS) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods SERVER_FOLDER_EXISTS
    importing
      value(IP_FOLDER) type CSEQUENCE
    returning
      value(R_EXISTS) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods UPLOAD_FILE_FROM_LOCAL
    importing
      value(IP_FILE_PATH) type CSEQUENCE
    returning
      value(R_FILE) type TY_S_FILE
    raising
      ZCX_GA_FILE .
  class-methods UPLOAD_FILE_FROM_SERVER
    importing
      value(IP_FILE_PATH) type CSEQUENCE
    returning
      value(R_FILE) type TY_S_FILE
    raising
      ZCX_GA_FILE .
  class-methods DOWNLOAD_FILE_TO_LOCAL
    importing
      value(IP_FILE_PATH) type CSEQUENCE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IP_FILE) type TY_S_FILE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
  class-methods DOWNLOAD_FILE_TO_SERVER
    importing
      value(IP_FILE_PATH) type CSEQUENCE
      value(IP_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IP_FILE) type TY_S_FILE
    returning
      value(R_BOOL) type ABAP_BOOL
    raising
      ZCX_GA_FILE .
ENDCLASS.



CLASS ZCL_GA_FILE IMPLEMENTATION.


  METHOD convert_x_tab.

    DATA:ld_file_size_int TYPE i.
    ld_file_size_int = lines( it_tab ).
    CHECK ld_file_size_int <> 0.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = ld_file_size_int
      IMPORTING
        buffer       = iop_file-xstring
      TABLES
        binary_tab   = it_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE zcx_ga_file
*        MESSAGE ID sy-msgid
*        TYPE sy-msgty
*        NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.
    iop_file-t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = iop_file-xstring ).
    iop_file-bytecount = xstrlen( iop_file-xstring ).
  ENDMETHOD.


  METHOD copy_file.
    TRY.
        IF ip_source_local = abap_true.
          IF ip_destination_local = abap_true.
            r_bool = copy_from_local_to_local( ip_source = ip_source ip_destination = ip_destination ip_overwrite = ip_overwrite ).
          ELSE.
            r_bool = copy_from_local_to_server( ip_source = ip_source ip_destination = ip_destination ip_overwrite = ip_overwrite ).
          ENDIF.
        ELSE.
          IF ip_destination_local = abap_true.
            r_bool = copy_from_server_to_local( ip_source = ip_source ip_destination = ip_destination ip_overwrite = ip_overwrite ).
          ELSE.
            r_bool = copy_from_server_to_server( ip_source = ip_source ip_destination = ip_destination ip_overwrite = ip_overwrite ).
          ENDIF.
        ENDIF.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD copy_from_local_to_local.
    TRY.
        IF NOT file_exists( ip_file = ip_source ip_local = abap_true ).
          zcx_ga_file=>raise_text( |File { ip_source } does not exist| ).
        ENDIF.
        DATA: ls_des_file TYPE ty_s_file.
        split_filename( EXPORTING ip_file_path = ip_destination
                        CHANGING iop_path = ls_des_file-path ).

        IF NOT folder_exists( ip_folder = ls_des_file-path-path ip_local = abap_true ).
          zcx_ga_file=>raise_text( |Local folder { ls_des_file-path-path } does not exist| ).
        ENDIF.
        r_bool = download_file_to_local(
                 ip_file_path = ip_destination
                 ip_overwrite = ip_overwrite
                 ip_file = upload_file_from_local( ip_file_path = ip_source )
        ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD copy_from_local_to_server.
    TRY.
        IF NOT file_exists( ip_file = ip_source ip_local = abap_true ).
          zcx_ga_file=>raise_text( |File { ip_source } does not exist| ).
        ENDIF.
        DATA: ls_des_file TYPE ty_s_file.

        split_filename( EXPORTING ip_file_path = ip_destination
                        CHANGING iop_path = ls_des_file-path ).

        IF NOT folder_exists( ip_folder = ls_des_file-path-path ip_local = abap_false ).
          zcx_ga_file=>raise_text( |SERVER folder { ls_des_file-path-path } does not exist| ).
        ENDIF.

        r_bool = download_file_to_server( ip_file_path     = ip_destination
                              ip_overwrite = ip_overwrite
                              ip_file = upload_file_from_local( ip_file_path = ip_source )
        ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD copy_from_server_to_local.
    TRY.
        IF NOT file_exists( ip_file = ip_source ip_local = abap_false ).
          zcx_ga_file=>raise_text( |File { ip_source } does not exist| ).
        ENDIF.
        DATA: ls_des_file TYPE ty_s_file.

        split_filename( EXPORTING ip_file_path = ip_destination
                         CHANGING iop_path = ls_des_file-path ).


        IF NOT folder_exists( ip_folder = ls_des_file-path-path ip_local = abap_true ).
          zcx_ga_file=>raise_text( |LOCAL folder { ls_des_file-path-path } does not exist| ).
        ENDIF.


        r_bool = download_file_to_local( ip_file_path = ip_destination
                             ip_overwrite = ip_overwrite
                             ip_file = upload_file_from_server( ip_file_path = ip_source )
        ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD copy_from_server_to_server.
    TRY.
        IF NOT file_exists( ip_file = ip_source ip_local = abap_false ).
          zcx_ga_file=>raise_text( |File { ip_source } does not exist| ).
        ENDIF.

        DATA: ls_des_file TYPE ty_s_file.

        split_filename(
          EXPORTING ip_file_path = ip_destination
          CHANGING iop_path = ls_des_file-path ).

        IF NOT folder_exists( ip_folder = ls_des_file-path-path ip_local = abap_false ).
          zcx_ga_file=>raise_text( |SERVER folder { ls_des_file-path-path } does not exist| ).
        ENDIF.

        r_bool = download_file_to_server( ip_file_path = ip_destination
                              ip_overwrite = ip_overwrite
                              ip_file = upload_file_from_server( ip_file_path = ip_source )
        ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_file.
    DATA ld_rc TYPE i.
    r_bool = abap_undefined.
    TRY.
        CASE ip_local.
          WHEN abap_true.
            r_bool = delete_file_at_local( ip_file ).
          WHEN abap_false.
            r_bool = delete_file_at_server( ip_file ).
        ENDCASE.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.


  ENDMETHOD.


  METHOD delete_file_at_local.
    DATA ld_rc TYPE i.
    r_bool = abap_undefined.
    CHECK local_file_exists( ip_file ).
    r_bool = abap_false.
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename             = CONV #( ip_file )
      CHANGING
        rc                   = ld_rc
      EXCEPTIONS
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.

    IF ld_rc = 0.
      r_bool = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD delete_file_at_server.
    DATA: lx_error TYPE REF TO cx_root.
    r_bool = abap_undefined.
    CHECK server_file_exists( ip_file ).
    r_bool = abap_false.

    TRY.
        DELETE DATASET ip_file.
        IF sy-subrc = 0.
          r_bool = abap_true.
        ENDIF.
        CLOSE DATASET ip_file.
      CATCH cx_sy_file_authority cx_sy_file_open INTO lx_error.
        zcx_ga_file=>raise_text( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD download_file.
*    DATA(ld_exists) = local_file_exists( ip_file_path ).
*    IF ip_overwrite = abap_false AND ld_exists = abap_true.
*      zcx_ga_file=>raise_text( |File already exists { ip_file_path }| ).
*    ENDIF.

*    IF ip_overwrite = abap_true AND ld_exists = abap_true.
*      delete_file_at_local( ip_file_path ).
*    ENDIF.
    TRY.
*        overwrite_file( ip_file = CONV #( ip_file_path ) ip_local = abap_true ip_overwrite = ip_overwrite ).
        CASE ip_local.
          WHEN abap_true.
            r_bool = download_file_to_local( ip_file_path = ip_file_path ip_overwrite = ip_overwrite ip_file = ip_file ).
          WHEN abap_false.
            r_bool = download_file_to_server( ip_file_path = ip_file_path ip_overwrite = ip_overwrite ip_file = ip_file ).
        ENDCASE.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

*    r_bool = file_exists( ip_file = ip_file_path ip_local = ip_local ).

  ENDMETHOD.


  METHOD download_file_to_local.

*    DATA(ld_exists) = local_file_exists( ip_file_path ).
*    IF ip_overwrite = abap_false AND ld_exists = abap_true.
*      zcx_ga_file=>raise_text( |File already exists { ip_file_path }| ).
*    ENDIF.

*    IF ip_overwrite = abap_true AND ld_exists = abap_true.
*      delete_file_at_local( ip_file_path ).
*    ENDIF.
    TRY.
        overwrite_file( ip_file = CONV #( ip_file_path ) ip_local = abap_true ip_overwrite = ip_overwrite ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = CONV #( ip_file_path )
        filetype                = 'BIN'
      CHANGING
        data_tab                = ip_file-t_rawdata
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      CLEAR r_bool.
      zcx_ga_file=>raise_symsg( ).

    ENDIF.

    r_bool = file_exists( ip_file = ip_file_path ip_local = abap_true ).

  ENDMETHOD.


  METHOD download_file_to_server.
*    DATA lx_error TYPE REF TO  cx_root.
*    DATA(ld_exists) = server_file_exists( ip_file_path ).
*
*    IF ip_overwrite = abap_false AND ld_exists = abap_true..
*      zcx_ga_file=>raise_text( |File already exists { ip_file_path }| ).
*    ENDIF.
*
*    IF ip_overwrite = abap_true AND ld_exists = abap_true.
*      delete_file_at_server( ip_file_path ).
*    ENDIF.
    TRY.
        overwrite_file( ip_file = ip_file_path ip_local = abap_false ip_overwrite = ip_overwrite ).
      CATCH zcx_ga_file INTO DATA(lx_ga_file).
        RAISE EXCEPTION lx_ga_file.
    ENDTRY.

    TRY.
        OPEN DATASET ip_file_path FOR OUTPUT IN BINARY MODE.
        IF sy-subrc = 0.
          TRANSFER ip_file-xstring TO ip_file_path.
          CLOSE DATASET ip_file_path.

        ENDIF.
      CATCH cx_sy_conversion_codepage  cx_sy_codepage_converter_init
            cx_sy_file_authority  cx_sy_file_io  cx_sy_file_open
            cx_sy_file_open_mode cx_sy_pipe_reopen
            cx_sy_too_many_files cx_sy_file_close
      INTO DATA(lx_error).

        zcx_ga_file=>raise_text( lx_error->get_text( ) ).

    ENDTRY.

    r_bool = file_exists( ip_file = ip_file_path ip_local = abap_false ).
  ENDMETHOD.


  METHOD f4_local_file.
    RAISE EXCEPTION TYPE zcx_ga_file
      EXPORTING
        error = |Método NO implementado todavía|.

  ENDMETHOD.


  METHOD f4_local_folder.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title         = CONV #( ip_window_title )
        initial_folder       = CONV #( ip_initial_folder )
      CHANGING
        selected_folder      = r_local_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.

  ENDMETHOD.


  METHOD file_exists.
*    r_exists = abap_true.
    r_exists = abap_false.
*    CALL METHOD cl_gui_frontend_services=>file_exist
*      EXPORTING
*        file                 = CONV #( ip_file )
*      RECEIVING
*        result               = r_exists
*      EXCEPTIONS
*        cntl_error           = 1
*        error_no_gui         = 2
*        wrong_parameter      = 3
*        not_supported_by_gui = 4
*        OTHERS               = 5.
*    IF sy-subrc <> 0.
*      zcx_ga_file=>raise_symsg( ).

*    ENDIF.
    TRY.
        CASE ip_local.
          WHEN abap_true.
            r_exists = local_file_exists( ip_file ).
          WHEN abap_false.
            r_exists = server_file_exists( ip_file ).
          WHEN OTHERS.
            r_exists = local_file_exists( ip_file ).
        ENDCASE.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD FOLDER_EXISTS.
    r_exists = abap_false.
    TRY.
        format_folder( CHANGING iop_folder = ip_folder ).
        CASE ip_local.
          WHEN abap_true.
            r_exists = local_folder_exists( ip_folder ).
          WHEN abap_false.
            r_exists = server_folder_exists( ip_folder ).
        ENDCASE.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD format_folder.
    DATA(ld_length) = strlen( iop_folder ) - 1.

    CHECK iop_folder+ld_length(1) <> '\'.
    iop_folder = |{ iop_folder  }\\|.

  ENDMETHOD.


  METHOD local_file_exists.
*    r_exists = abap_true.
    r_exists = abap_false.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = CONV #( ip_file )
      RECEIVING
        result               = r_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).

    ENDIF.

  ENDMETHOD.


  METHOD local_folder_exists.
    r_exists = abap_false.
    format_folder( CHANGING iop_folder = ip_folder ).
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = ip_folder
      RECEIVING
        result               = r_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.


  ENDMETHOD.


  METHOD overwrite_file.
    TRY.

        IF file_exists( EXPORTING ip_file = CONV #( ip_file ) ip_local = ip_local ) = abap_true.
          IF ip_overwrite = abap_false.
            RAISE EXCEPTION TYPE zcx_ga_file
              EXPORTING
                error = |File already exists: { ip_file }|.
          ELSE.
            IF delete_file( ip_file = ip_file ) = abap_false.
              RAISE EXCEPTION TYPE zcx_ga_file
                EXPORTING
                  error = |Original File could NOT be deleted: { ip_file }|.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD read_csv.

    TRY.

        IF file_exists( EXPORTING ip_file = CONV #( ip_file ) ip_local = ip_local ) = abap_false.
          RAISE EXCEPTION TYPE zcx_ga_file
            EXPORTING
              error = |File { ip_file } does NOT exist|.
        ENDIF.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    DATA lo_csv_file TYPE REF TO zcl_wd_csv_file.

    lo_csv_file = NEW #( encoding = ip_encoding
                         replacement = ip_replacement
                         ignore_cerr = ip_ignore_cerr
                         conv_exit = ip_conv_exit
                         trim_spaces = ip_trim_spaces

                         endofline = ip_endofline
                         separator = ip_separator
                         delimiter = ip_delimiter ).

    TRY.


        CASE ip_local.
          WHEN abap_true.
            lo_csv_file->parse_file_local(
                EXPORTING has_header = ip_has_header
                          path = CONV #( ip_file )
                IMPORTING target_table = iot_target_table
              ).
          WHEN abap_false.
            lo_csv_file->parse_file_appl(
              EXPORTING has_header = ip_has_header
                        path = CONV #( ip_file )
              IMPORTING target_table = iot_target_table
            ).
        ENDCASE.
      CATCH cx_sy_struct_creation cx_sy_conversion_no_number cx_sy_conversion_error
            cx_sy_file_open cx_sy_codepage_converter_init
            cx_sy_file_authority cx_sy_file_io
            cx_sy_file_open_mode cx_sy_file_close
            cx_parameter_invalid_range cx_parameter_invalid_type
            zcx_wd_csv_too_many_columns zcx_wd_csv_too_few_columns
            zcx_wd_csv_mixed_endofline cx_root INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_ex->get_longtext( ) ).


    ENDTRY.
  ENDMETHOD.


  METHOD read_xlsx.
    DEFINE get_cell.
      &1->get_cell( EXPORTING ip_column = &2 ip_row = &3
      IMPORTING ep_value = &4 ).
    END-OF-DEFINITION.
    TRY.

        IF file_exists( EXPORTING ip_file = CONV #( ip_file ) ip_local = ip_local ) = abap_false.
          RAISE EXCEPTION TYPE zcx_ga_file
            EXPORTING
              error = |File { ip_file } does not exist|.
        ENDIF.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
    DATA: lo_excel  TYPE REF TO zcl_excel,
          lo_reader TYPE REF TO zif_excel_reader.
    DATA: ld_file TYPE string.
    TRY .

        CREATE OBJECT lo_reader TYPE zcl_excel_reader_2007.
        lo_excel = lo_reader->load_file( i_filename = CONV #( ip_file )
        i_from_applserver = COND #( WHEN ip_local = abap_true THEN abap_false
                                    WHEN ip_local = abap_false THEN abap_true
                                    ELSE abap_false )
        ).


      CATCH zcx_excel.
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = |Error reading file|.
    ENDTRY.
    CHECK lo_excel IS BOUND.
    FIELD-SYMBOLS: <lf_table> TYPE table,
                   <lf_row>   TYPE any.
    ASSIGN iot_target_table[] TO <lf_table>.
    DATA: lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          lo_columns    TYPE REF TO cl_abap_structdescr.
    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( <lf_table> )."->GET_TABLE_LINE_TYPE( )
    lo_columns ?= lo_tabledescr->get_table_line_type( ).
    DATA: lo_row TYPE REF TO data.
    CREATE DATA lo_row TYPE HANDLE lo_columns.
    ASSIGN lo_row->* TO <lf_row>.
    DATA: lt_columns     TYPE abap_component_tab,
          lt_aux_columns TYPE abap_component_tab,
          ls_column      TYPE abap_componentdescr,
          lo_wks         TYPE REF TO zcl_excel_worksheet,
          ld_cell_value  TYPE zexcel_cell_value,
          ld_column      TYPE i,
          ld_row         TYPE i.
    lt_aux_columns = lo_columns->get_components( ).
    lo_wks = lo_excel->get_active_worksheet( ).
    ld_row = ip_start_row.
*    loop at lt_column
    get_components( EXPORTING io_structure = lo_columns
                     CHANGING iot_columns = lt_columns ).
    FIELD-SYMBOLS:
      <lf_line>  TYPE any,
      <lf_field> TYPE any.
    DO.
      ld_column = ip_start_column.
      CLEAR <lf_row>.
      LOOP AT lt_columns INTO ls_column.
        ADD 1 TO ld_column.
        ASSIGN COMPONENT ls_column-name OF STRUCTURE <lf_row> TO <lf_field>.
        IF <lf_field> IS ASSIGNED.
          TRY.
              get_cell lo_wks ld_column ld_row ld_cell_value.
            CATCH zcx_excel.
              RAISE EXCEPTION TYPE zcx_ga_file
                EXPORTING
                  error = |Error Reading cell { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
          ENDTRY.

          CASE cl_abap_datadescr=>get_data_type_kind( <lf_field> ).
            WHEN cl_abap_typedescr=>typekind_date.
              TRY.
                  <lf_field> = zcl_excel_common=>excel_string_to_date( ip_value = ld_cell_value ).
                CATCH zcx_excel
                  cx_sy_conversion_error.
                  RAISE EXCEPTION TYPE zcx_ga_file
                    EXPORTING
                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS DATE|.
              ENDTRY.
            WHEN cl_abap_typedescr=>typekind_time.
              TRY.
                  <lf_field> = zcl_excel_common=>excel_string_to_time( ip_value = ld_cell_value ).
                CATCH zcx_excel
                  cx_sy_conversion_error.
                  RAISE EXCEPTION TYPE zcx_ga_file
                    EXPORTING
                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS TIME|.
              ENDTRY.
            WHEN cl_abap_typedescr=>typekind_char OR
              cl_abap_typedescr=>typekind_csequence OR
              cl_abap_typedescr=>typekind_string. "tipo caracter
              TRY.
                  <lf_field> = ld_cell_value.
                CATCH cx_sy_conversion_error.
                  RAISE EXCEPTION TYPE zcx_ga_file
                    EXPORTING
                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS string|.
              ENDTRY.

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
              TRY.
                  <lf_field> = CONV #( ld_cell_value ).
                CATCH cx_sy_conversion_error.
                  RAISE EXCEPTION TYPE zcx_ga_file
                    EXPORTING
                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS NUMBER|.
              ENDTRY.
            WHEN OTHERS. "tipo numero.
              TRY.
                  <lf_field> = CONV #( ld_cell_value ).
                CATCH cx_sy_conversion_error.
                  RAISE EXCEPTION TYPE zcx_ga_file
                    EXPORTING
                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
              ENDTRY.
          ENDCASE.

*
        ENDIF.
      ENDLOOP.
      IF <lf_row> IS INITIAL.
        EXIT.
      ENDIF.
      APPEND <lf_row> TO <lf_table>.
      ADD 1 TO ld_row.
    ENDDO.
  ENDMETHOD.


  METHOD server_file_exists.
    DATA: lx_error TYPE REF TO cx_root.
    r_exists = abap_false.
    TRY.
        OPEN DATASET ip_file FOR INPUT IN BINARY MODE.
        IF sy-subrc = 0.
          r_exists = abap_true.
          CLOSE DATASET ip_file.
        ENDIF.
      CATCH cx_sy_file_access_error
            cx_sy_pipes_not_supported cx_sy_too_many_files
       INTO lx_error.
        zcx_ga_file=>raise_text( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD server_folder_exists.
    r_exists = abap_false.
    format_folder( CHANGING iop_folder = ip_folder ).
    DATA: ld_file TYPE string.
    ld_file = |{ ip_folder }test|.
    TRY.
        OPEN DATASET ld_file FOR OUTPUT IN BINARY MODE.
        IF sy-subrc = 0.
          r_exists = abap_true.
          CLOSE DATASET ld_file.
        ENDIF.
      CATCH cx_sy_file_open cx_sy_codepage_converter_init cx_sy_conversion_codepage
        cx_sy_file_authority cx_sy_pipes_not_supported cx_sy_too_many_files
        cx_sy_file_close .
        r_exists = abap_false.
    ENDTRY.

    CHECK r_exists = abap_true.
    TRY.
        delete_file_at_server( ld_file ).
      CATCH zcx_ga_file.
        r_exists = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD split_filename.
    CALL FUNCTION 'CH_SPLIT_FILENAME'
      EXPORTING
        complete_filename = CONV string( ip_file_path )
      IMPORTING
        drive             = iop_path-drive
        extension         = iop_path-extension
        name              = iop_path-name
        name_with_ext     = iop_path-name_ext
        path              = iop_path-path
      EXCEPTIONS
        invalid_drive     = 1
        invalid_path      = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.
    format_folder( CHANGING iop_folder = iop_path-path ).
  ENDMETHOD.


  METHOD upload_file.
**    CHECK file_exists( ip_file = ip_file_path ip_local = abap_true ) = abap_true.
    TRY.
        CASE ip_local.
          WHEN abap_true.
            r_file = upload_file_from_local( ip_file_path ).
          WHEN abap_false.
            r_file = upload_file_from_server( ip_file_path ).
        ENDCASE.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD upload_file_from_local.
    CHECK file_exists( ip_file = ip_file_path ip_local = abap_true ) = abap_true.

    DATA lt_tab TYPE ty_t_xtab.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = ip_file_path
        filetype                = 'BIN'
      CHANGING
        data_tab                = lt_tab[]
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.

    convert_x_tab( EXPORTING it_tab = lt_tab CHANGING iop_file = r_file ).

    split_filename( EXPORTING ip_file_path = ip_file_path CHANGING iop_path = r_file-path ).
  ENDMETHOD.


  METHOD upload_file_from_server.
    DATA lx_error TYPE REF TO cx_root.

    CHECK file_exists( ip_file = ip_file_path ip_local = abap_false ).

    DATA: lt_tab TYPE ty_t_xtab,
          ls_tab TYPE LINE OF ty_t_xtab.

    TRY.
        OPEN DATASET ip_file_path FOR INPUT IN BINARY MODE.
        CHECK sy-subrc = 0.
        DO.
          CLEAR ls_tab.
          READ DATASET ip_file_path INTO ls_tab.
          IF sy-subrc = 0..
            APPEND ls_tab TO lt_tab.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
        CLOSE DATASET ip_file_path .
      CATCH cx_sy_file_open cx_sy_codepage_converter_init
            cx_sy_conversion_codepage cx_sy_file_authority
            cx_sy_pipes_not_supported cx_sy_too_many_files
            cx_sy_file_io cx_sy_file_open_mode cx_sy_pipe_reopen
            cx_sy_file_close
        INTO lx_error.
        zcx_ga_file=>raise_text( lx_error->get_text( ) ).
    ENDTRY.
    convert_x_tab( EXPORTING it_tab = lt_tab CHANGING iop_file = r_file ).
    split_filename( EXPORTING ip_file_path = ip_file_path CHANGING iop_path = r_file-path ).

  ENDMETHOD.


  METHOD write_csv.
    TRY.
        overwrite_file( ip_file = ip_file ip_local = ip_local ip_overwrite = ip_overwrite ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
    DATA lo_csv_file TYPE REF TO zcl_wd_csv_file.

    lo_csv_file = NEW #( encoding = ip_encoding
                         replacement = ip_replacement
                         ignore_cerr = ip_ignore_cerr
                         conv_exit = ip_conv_exit
                         trim_spaces = ip_trim_spaces

                         endofline = ip_endofline
                         separator = ip_separator
                         delimiter = ip_delimiter ).

    TRY.

        CASE ip_local.
          WHEN abap_true.
            lo_csv_file->generate_file_local(
                EXPORTING with_header = ip_with_header
                          path = CONV #( ip_file )
                          source_table = it_target_table
              ).
          WHEN abap_false.
            lo_csv_file->generate_file_appl(
              EXPORTING with_header = ip_with_header
                        path = CONV #( ip_file )
                        source_table = it_target_table
            ).
        ENDCASE.

      CATCH cx_sy_struct_creation cx_sy_conversion_error
            cx_sy_file_open cx_sy_codepage_converter_init
            cx_sy_file_authority cx_sy_file_io
            cx_sy_file_open_mode cx_sy_file_close
            cx_parameter_invalid_range cx_parameter_invalid_type
            zcx_wd_csv_too_many_columns zcx_wd_csv_too_few_columns
           zcx_wd_csv_mixed_endofline INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_ex->get_longtext( ) ).


    ENDTRY.
  ENDMETHOD.


  METHOD write_xlsx.
    TRY.
        overwrite_file( ip_file = ip_file ip_local = ip_local ip_overwrite = ip_overwrite ).
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
*

    DATA: lo_file TYPE REF TO zcl_eui_file_io.
    TRY.
        lo_file ?= NEW zcl_eui_file_io( iv_file_name = CONV #( ip_file ) ).
        lo_file->import_from_itab( ir_table = REF #( it_source_table ) ).
      CATCH zcx_eui_exception INTO DATA(lx_eui).
        zcx_ga_file=>raise_text( CONV #( lx_eui->get_longtext( ) ) ).

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
