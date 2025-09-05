CLASS zcl_ga_file DEFINITION
  PUBLIC
  INHERITING FROM zcl_ga_util
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_t_xtab TYPE TABLE OF x .
    TYPES:
      BEGIN OF ty_s_file_path,
        drive        TYPE pcfile-drive,
        extension    TYPE string,
        name         TYPE string,
        name_ext     TYPE string,
        path         TYPE string,
        fullpath     TYPE string,
        is_directory TYPE abap_bool,
      END OF ty_s_file_path .
    TYPES:
      BEGIN OF ty_s_file,
        path      TYPE ty_s_file_path,
        xstring   TYPE xstring,
        t_rawdata TYPE solix_tab,
        bytecount TYPE i,
      END OF ty_s_file .
    TYPES:
      ty_t_files_path TYPE TABLE OF ty_s_file_path WITH DEFAULT KEY .

    CLASS-DATA user_parameter_local_home TYPE memoryid READ-ONLY VALUE 'CTP' ##NO_TEXT.
    CLASS-DATA local_home TYPE string .
    CLASS-DATA local_file_separator TYPE char1 .

    CLASS-METHODS get_encoding
      IMPORTING
        VALUE(ip_file)     TYPE string
        VALUE(ip_local)    TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rt_encoding) TYPE abap_encod .
    CLASS-METHODS convert_string_to_file
      IMPORTING
        VALUE(ip_string) TYPE string
      CHANGING
        VALUE(iop_file)  TYPE ty_s_file .
    CLASS-METHODS convert_x_tab
      IMPORTING
        VALUE(it_tab)   TYPE ty_t_xtab
      CHANGING
        VALUE(iop_file) TYPE ty_s_file .
    CLASS-METHODS remove_environmental_val
      IMPORTING
        VALUE(ip_path) TYPE string
      RETURNING
        VALUE(r_path)  TYPE string .
    CLASS-METHODS get_local_home
      RETURNING
        VALUE(r_local_home) TYPE string .
    CLASS-METHODS get_dummy_file
      RETURNING
        VALUE(r_dummy_file) TYPE ty_s_file .
    CLASS-METHODS randomize_filename
      IMPORTING
        VALUE(ip_path)   TYPE string
        VALUE(ip_ext)    TYPE string
        VALUE(ip_local)  TYPE abap_bool DEFAULT abap_true
        VALUE(ip_prefix) TYPE string DEFAULT 'rand'
        VALUE(ip_length) TYPE i DEFAULT 4
      RETURNING
        VALUE(r_file)    TYPE string
      RAISING
        zcx_ga_file .
    CLASS-METHODS copy_file
      IMPORTING
        VALUE(ip_destination)       TYPE csequence
        VALUE(ip_destination_local) TYPE abap_bool DEFAULT abap_true
        VALUE(ip_overwrite)         TYPE abap_bool DEFAULT abap_false
        VALUE(ip_source)            TYPE csequence
        VALUE(ip_source_local)      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_bool)               TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS format_folder
      IMPORTING
        VALUE(ip_local)  TYPE abap_bool DEFAULT abap_true
        VALUE(ip_folder) TYPE csequence
      RETURNING
        VALUE(r_folder)  TYPE string .
    CLASS-METHODS delete_file
      IMPORTING
        VALUE(ip_file)  TYPE csequence
        VALUE(ip_local) TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_bool)   TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS split_filename
      IMPORTING
        VALUE(ip_file_path) TYPE csequence
        VALUE(ip_local)     TYPE abap_bool DEFAULT abap_true
      CHANGING
        VALUE(iop_path)     TYPE ty_s_file_path
      RAISING
        zcx_ga_file .
    CLASS-METHODS f4_local_folder
      IMPORTING
        VALUE(ip_window_title)   TYPE csequence OPTIONAL
        VALUE(ip_initial_folder) TYPE csequence OPTIONAL
      RETURNING
        VALUE(r_local_folder)    TYPE string
      RAISING
        zcx_ga_file .
    CLASS-METHODS f4_local_file
      RAISING
        zcx_ga_file .
    CLASS-METHODS file_exists
      IMPORTING
        VALUE(ip_file)  TYPE csequence
        VALUE(ip_local) TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_exists) TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS folder_is_writable
      IMPORTING
        VALUE(ip_local)      TYPE abap_bool DEFAULT abap_true
      CHANGING
        VALUE(ip_folder)     TYPE csequence
      RETURNING
        VALUE(r_is_writable) TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS folder_exists
      IMPORTING
        VALUE(ip_local)  TYPE abap_bool DEFAULT abap_true
      CHANGING
        VALUE(ip_folder) TYPE csequence
      RETURNING
        VALUE(r_exists)  TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS upload_file
      IMPORTING
        VALUE(ip_file_path) TYPE csequence
        VALUE(ip_local)     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_file)       TYPE ty_s_file
      RAISING
        zcx_ga_file .
    CLASS-METHODS download_file
      IMPORTING
        VALUE(ip_file_path) TYPE csequence
        VALUE(ip_local)     TYPE abap_bool DEFAULT abap_false
        VALUE(ip_overwrite) TYPE abap_bool DEFAULT abap_false
        VALUE(ip_file)      TYPE ty_s_file
      RETURNING
        VALUE(r_bool)       TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS read_csv
      IMPORTING
        !ip_file              TYPE string
        VALUE(ip_local)       TYPE abap_bool DEFAULT abap_true
        VALUE(ip_has_header)  TYPE abap_bool DEFAULT abap_false
        VALUE(ip_endofline)   TYPE csequence DEFAULT zcl_wd_csv=>c_endofline_cr_lf
        VALUE(ip_delimiter)   TYPE char1 DEFAULT '"'
        VALUE(ip_separator)   TYPE char1 DEFAULT cl_abap_char_utilities=>horizontal_tab
        VALUE(ip_encoding)    TYPE abap_encod OPTIONAL
        VALUE(ip_replacement) TYPE abap_repl DEFAULT '#'
        VALUE(ip_conv_exit)   TYPE abap_bool DEFAULT abap_false
        VALUE(ip_trim_spaces) TYPE abap_bool DEFAULT abap_false
        VALUE(ip_ignore_cerr) TYPE abap_bool DEFAULT abap_true
      CHANGING
        !iot_target_table     TYPE STANDARD TABLE
      RAISING
        zcx_ga_file .
    CLASS-METHODS read_xlsx
      IMPORTING
        !ip_file                    TYPE string
        VALUE(ip_local)             TYPE abap_bool DEFAULT abap_true
        VALUE(ip_has_header)        TYPE abap_bool DEFAULT abap_false
        VALUE(ip_start_row)         TYPE int1 DEFAULT 2
        VALUE(ip_start_column)      TYPE int1 DEFAULT 0
        VALUE(ip_wks_title)         TYPE csequence OPTIONAL
        VALUE(ip_apply_conv)        TYPE abap_bool DEFAULT abap_false
        VALUE(ip_continue_on_error) TYPE abap_bool DEFAULT abap_false
        VALUE(ip_show_error)        TYPE abap_bool DEFAULT abap_false
        VALUE(ip_date_pattern)      TYPE string OPTIONAL
        VALUE(io_xlsx_reader)       TYPE REF TO zif_excel_reader OPTIONAL
      CHANGING
        !iot_target_table           TYPE STANDARD TABLE
      RETURNING
        VALUE(r_ok)                 TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS write_xlsx
      IMPORTING
        !ip_file                TYPE string
        VALUE(ip_local)         TYPE abap_bool DEFAULT abap_true
        VALUE(ip_overwrite)     TYPE abap_bool DEFAULT abap_false
        VALUE(ip_has_header)    TYPE abap_bool DEFAULT abap_false
        VALUE(ip_start_row)     TYPE int1 DEFAULT 2
        VALUE(ip_start_column)  TYPE int1 DEFAULT 0
        VALUE(ip_use_abap_xlsx) TYPE abap_bool DEFAULT abap_false
        VALUE(it_source_table)  TYPE STANDARD TABLE
      RAISING
        zcx_ga_file .
    CLASS-METHODS server_directory_list_enh
      IMPORTING
        VALUE(ip_folder)       TYPE csequence
        VALUE(ip_filter)       TYPE csequence DEFAULT '*.*'
        VALUE(ip_files_only)   TYPE abap_bool DEFAULT abap_false
        VALUE(ip_folders_only) TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_files)        TYPE ty_t_files_path
      RAISING
        zcx_ga_file .
    CLASS-METHODS write_csv
      IMPORTING
        !ip_file              TYPE string
        VALUE(ip_local)       TYPE abap_bool DEFAULT abap_true
        VALUE(ip_overwrite)   TYPE abap_bool DEFAULT abap_false
        VALUE(ip_with_header) TYPE abap_bool DEFAULT abap_false
        VALUE(ip_endofline)   TYPE csequence DEFAULT zcl_wd_csv=>c_endofline_cr_lf
        VALUE(ip_delimiter)   TYPE char1 DEFAULT '"'
        VALUE(ip_separator)   TYPE char1 DEFAULT cl_abap_char_utilities=>horizontal_tab
        VALUE(ip_encoding)    TYPE abap_encod DEFAULT '4110'
        VALUE(ip_replacement) TYPE abap_repl DEFAULT '#'
        VALUE(ip_conv_exit)   TYPE abap_bool DEFAULT abap_false
        VALUE(ip_trim_spaces) TYPE abap_bool DEFAULT abap_false
        VALUE(ip_ignore_cerr) TYPE abap_bool DEFAULT abap_true
        !it_target_table      TYPE STANDARD TABLE
      RAISING
        zcx_ga_file .
    CLASS-METHODS directory_list
      IMPORTING
        VALUE(ip_folder)       TYPE csequence
        VALUE(ip_local)        TYPE abap_bool DEFAULT abap_false
        VALUE(ip_filter)       TYPE csequence DEFAULT '*.*'
        VALUE(ip_files_only)   TYPE abap_bool DEFAULT abap_false
        VALUE(ip_folders_only) TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_files)        TYPE ty_t_files_path
      RAISING
        zcx_ga_file .
    CLASS-METHODS read_xlsx_as_alsm
      IMPORTING
        VALUE(ip_filename)  TYPE csequence
        VALUE(ip_local)     TYPE abap_bool DEFAULT abap_true
        VALUE(ip_begin_col) TYPE i
        VALUE(ip_begin_row) TYPE i
        VALUE(ip_end_col)   TYPE i
        VALUE(ip_end_row)   TYPE i
      RETURNING
        VALUE(rt_table)     TYPE isu_price_excel_tab
      RAISING
        zcx_ga_util .
  PROTECTED SECTION.

    CLASS-METHODS write_xlsx_abap
      IMPORTING
        VALUE(ip_file)         TYPE string
        VALUE(ip_overwrite)    TYPE abap_bool DEFAULT abap_false
        VALUE(ip_local)        TYPE abap_bool DEFAULT abap_true
        VALUE(ip_start_row)    TYPE int1 DEFAULT 2
        VALUE(ip_start_column) TYPE int1 DEFAULT 0
        VALUE(it_source_table) TYPE STANDARD TABLE .
  PRIVATE SECTION.

    CLASS-METHODS get_encoding_local
      IMPORTING
        VALUE(ip_file)     TYPE string
      RETURNING
        VALUE(rt_encoding) TYPE abap_encod .
    CLASS-METHODS get_encoding_server
      IMPORTING
        VALUE(ip_file)     TYPE string
      RETURNING
        VALUE(rt_encoding) TYPE abap_encod .
    CLASS-METHODS format_folder_server
      IMPORTING
        VALUE(ip_folder) TYPE csequence
      RETURNING
        VALUE(r_folder)  TYPE string .
    CLASS-METHODS format_folder_local
      IMPORTING
        VALUE(ip_folder) TYPE csequence
      RETURNING
        VALUE(r_folder)  TYPE string .
    CLASS-METHODS overwrite_file
      IMPORTING
        VALUE(ip_overwrite) TYPE abap_bool DEFAULT abap_false
        VALUE(ip_file)      TYPE csequence
        VALUE(ip_local)     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_bool)       TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS delete_file_at_local
      IMPORTING
        VALUE(ip_file) TYPE csequence
      RETURNING
        VALUE(r_bool)  TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS delete_file_at_server
      IMPORTING
        VALUE(ip_file) TYPE csequence
      RETURNING
        VALUE(r_bool)  TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS copy_from_server_to_server
      IMPORTING
        VALUE(ip_source)      TYPE csequence
        VALUE(ip_destination) TYPE csequence
        VALUE(ip_overwrite)   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_bool)         TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS copy_from_server_to_local
      IMPORTING
        VALUE(ip_source)      TYPE csequence
        VALUE(ip_destination) TYPE csequence
        VALUE(ip_overwrite)   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_bool)         TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS copy_from_local_to_server
      IMPORTING
        VALUE(ip_source)      TYPE csequence
        VALUE(ip_destination) TYPE csequence
        VALUE(ip_overwrite)   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_bool)         TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS copy_from_local_to_local
      IMPORTING
        VALUE(ip_source)      TYPE csequence
        VALUE(ip_destination) TYPE csequence
        VALUE(ip_overwrite)   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_bool)         TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS local_file_exists
      IMPORTING
        VALUE(ip_file)  TYPE csequence
      RETURNING
        VALUE(r_exists) TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS server_file_exists
      IMPORTING
        VALUE(ip_file)  TYPE csequence
      RETURNING
        VALUE(r_exists) TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS local_folder_exists
      IMPORTING
        VALUE(ip_folder) TYPE csequence
      RETURNING
        VALUE(r_exists)  TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS local_folder_is_writable
      IMPORTING
        VALUE(ip_folder)     TYPE csequence
      RETURNING
        VALUE(r_is_writable) TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS server_folder_exists
      IMPORTING
        VALUE(ip_folder) TYPE csequence
      RETURNING
        VALUE(r_exists)  TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS upload_file_from_local
      IMPORTING
        VALUE(ip_file_path) TYPE csequence
      RETURNING
        VALUE(r_file)       TYPE ty_s_file
      RAISING
        zcx_ga_file .
    CLASS-METHODS upload_file_from_server
      IMPORTING
        VALUE(ip_file_path) TYPE csequence
      RETURNING
        VALUE(r_file)       TYPE ty_s_file
      RAISING
        zcx_ga_file .
    CLASS-METHODS download_file_to_local
      IMPORTING
        VALUE(ip_file_path)     TYPE csequence
        VALUE(ip_overwrite)     TYPE abap_bool DEFAULT abap_false
        VALUE(ip_file)          TYPE ty_s_file
        VALUE(ip_show_messages) TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_bool)           TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS download_file_to_server
      IMPORTING
        VALUE(ip_file_path) TYPE csequence
        VALUE(ip_overwrite) TYPE abap_bool DEFAULT abap_false
        VALUE(ip_file)      TYPE ty_s_file
      RETURNING
        VALUE(r_bool)       TYPE abap_bool
      RAISING
        zcx_ga_file .
    CLASS-METHODS local_directory_list
      IMPORTING
        VALUE(ip_folder)       TYPE csequence
        VALUE(ip_filter)       TYPE csequence DEFAULT '*.*'
        VALUE(ip_files_only)   TYPE abap_bool DEFAULT abap_false
        VALUE(ip_folders_only) TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_files)        TYPE ty_t_files_path
      RAISING
        zcx_ga_file .
    CLASS-METHODS server_directory_list
      IMPORTING
        VALUE(ip_folder)       TYPE csequence
        VALUE(ip_filter)       TYPE csequence DEFAULT '*.*'
        VALUE(ip_files_only)   TYPE abap_bool DEFAULT abap_false
        VALUE(ip_folders_only) TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_files)        TYPE ty_t_files_path
      RAISING
        zcx_ga_file .
ENDCLASS.



CLASS zcl_ga_file IMPLEMENTATION.


  METHOD convert_string_to_file.
    TRY.
        DATA(lt_soli) = cl_bcs_convert=>string_to_soli( ip_string ).
        iop_file-t_rawdata = cl_bcs_convert=>soli_to_solix( lt_soli ).
        iop_file-xstring = cl_bcs_convert=>solix_to_xstring( iop_file-t_rawdata ).
        iop_file-bytecount = xstrlen( iop_file-xstring ).
      CATCH cx_bcs.
    ENDTRY.
  ENDMETHOD.


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
*      zcx_ga_file=>raise_symsg( ).
      RAISE EXCEPTION TYPE zcx_ga_file
        EXPORTING
          syst_at_raise = syst.
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
*          zcx_ga_file=>raise_text( |File { ip_source } does not exist| ).
          get_singleton( )->log_e( |File { ip_source } does not exist|  ).
          EXIT.
        ENDIF.
        DATA: ls_des_file TYPE ty_s_file.
        split_filename( EXPORTING ip_file_path = ip_destination
                        CHANGING iop_path = ls_des_file-path ).

        IF NOT folder_exists( EXPORTING ip_local = abap_true CHANGING ip_folder = ls_des_file-path-path ).
*          zcx_ga_file=>raise_text( |Local folder { ls_des_file-path-path } does not exist| ).
          get_singleton( )->log_e( |Local folder { ls_des_file-path-path } does not exist| ).
          EXIT.
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
*          zcx_ga_file=>raise_text( |File { ip_source } does not exist| ).
          get_singleton( )->log_e(  |File { ip_source } does not exist| ).
          EXIT.
        ENDIF.
        DATA: ls_des_file TYPE ty_s_file.

        split_filename( EXPORTING ip_file_path = ip_destination
                                 ip_local = abap_false
                        CHANGING iop_path = ls_des_file-path ).

        IF NOT folder_exists( EXPORTING  ip_local = abap_false CHANGING ip_folder = ls_des_file-path-path ).
*          zcx_ga_file=>raise_text( |SERVER folder { ls_des_file-path-path } does not exist| ).
          get_singleton( )->log_e( |SERVER folder { ls_des_file-path-path } does not exist| ).
          EXIT.
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
          get_singleton( )->log_e( |File { ip_source } does not exist| ).
          EXIT.
        ENDIF.
        DATA: ls_des_file TYPE ty_s_file.

        split_filename( EXPORTING ip_file_path = ip_destination
                         CHANGING iop_path = ls_des_file-path ).


        IF NOT folder_exists( EXPORTING ip_local = abap_true CHANGING ip_folder = ls_des_file-path-path ).
          get_singleton( )->log_e( |LOCAL folder { ls_des_file-path-path } does not exist| ).
          EXIT.
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
*          zcx_ga_file=>raise_text( |File { ip_source } does not exist| ).
          get_singleton( )->log_e( |File { ip_source } does not exist| ).
          EXIT.
        ENDIF.

        DATA: ls_des_file TYPE ty_s_file.

        split_filename(
          EXPORTING ip_file_path = ip_destination
                    ip_local = abap_false
          CHANGING iop_path = ls_des_file-path ).

        IF NOT folder_exists( EXPORTING ip_local = abap_false CHANGING ip_folder = ls_des_file-path-path ).
*          zcx_ga_file=>raise_text( |SERVER folder { ls_des_file-path-path } does not exist| ).
          get_singleton( )->log_e( |SERVER folder { ls_des_file-path-path } does not exist| ).
          EXIT.
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
*      zcx_ga_file=>raise_symsg( ).
      RAISE EXCEPTION TYPE zcx_ga_file
        EXPORTING
          syst_at_raise = syst.
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
*        zcx_ga_file=>raise_text( lx_error->get_text( ) ).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD directory_list.
    ip_folder = format_folder( EXPORTING ip_folder = ip_folder ip_local = ip_local ).
    rt_files = SWITCH #( ip_local
        WHEN abap_true THEN local_directory_list( ip_folder = ip_folder
                                                  ip_filter = ip_filter
                                                  ip_files_only = ip_files_only
                                                  ip_folders_only = ip_folders_only
                                                 )
        WHEN abap_false THEN server_directory_list( ip_folder = ip_folder
                                                    ip_filter = ip_filter
                                                    ip_files_only = ip_files_only
                                                    ip_folders_only = ip_folders_only
                                                   )
    ).

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
        show_transfer_status    = ip_show_messages
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
*      zcx_ga_file=>raise_symsg( ).
      RAISE EXCEPTION TYPE zcx_ga_file
        EXPORTING
          syst_at_raise = syst.

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

*        zcx_ga_file=>raise_text( lx_error->get_text( ) ).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_error->get_text( ) ).

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
*      zcx_ga_file=>raise_symsg( ).
      RAISE EXCEPTION TYPE zcx_ga_file
        EXPORTING
          syst_at_raise = syst.
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


  METHOD folder_exists.
    r_exists = abap_false.
    TRY.
        ip_folder = format_folder( EXPORTING ip_local = ip_local ip_folder = ip_folder ).
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


  METHOD folder_is_writable.
    r_is_writable = abap_false.
    CHECK ip_folder IS NOT INITIAL.
    TRY.
        ip_folder = format_folder( EXPORTING ip_local = ip_local ip_folder = ip_folder   ).
        CASE ip_local.
          WHEN abap_true.
            r_is_writable = local_folder_is_writable( ip_folder ).
          WHEN abap_false.
            r_is_writable = server_folder_exists( ip_folder ).
        ENDCASE.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD format_folder.
    CHECK ip_folder IS NOT INITIAL.
    CASE ip_local.
      WHEN abap_true.
        r_folder = format_folder_local( ip_folder ).
      WHEN abap_false.
        r_folder = format_folder_server( ip_folder ).
    ENDCASE.


  ENDMETHOD.


  METHOD format_folder_local.



    IF local_file_separator IS INITIAL.
      cl_gui_frontend_services=>get_file_separator(
            CHANGING file_separator = local_file_separator
          EXCEPTIONS not_supported_by_gui = 1
                     error_no_gui = 2
                     cntl_error = 3
        ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.
    r_folder = remove_environmental_val( ip_path = CONV #( ip_folder ) ).
    DATA(ld_length) = strlen( r_folder ) - 1.
    CHECK r_folder+ld_length(1) <> local_file_separator.
    r_folder = |{ r_folder  }{ local_file_separator }|.
  ENDMETHOD.


  METHOD format_folder_server.
    r_folder = ip_folder.
    DATA(ld_length) = strlen( r_folder ) - 1.
    CHECK r_folder+ld_length(1) <> '\'.
    r_folder = |{ r_folder  }\\|.
  ENDMETHOD.


  METHOD get_dummy_file.
    r_dummy_file-xstring = cl_bcs_convert=>string_to_xstring( EXPORTING iv_string = 'This is a dummy file').
    r_dummy_file-t_rawdata = cl_bcs_convert=>xstring_to_solix( EXPORTING iv_xstring  = r_dummy_file-xstring ).
    r_dummy_file-bytecount = xstrlen( r_dummy_file-xstring ).
  ENDMETHOD.


  METHOD get_encoding.
*    rt_encoding = SWITCH #( ip_local
*                  WHEN abap_true THEN get_encoding_local( ip_file )
*                  WHEN abap_false THEN get_encoding_server( ip_file )
*                  ).

    DATA(ls_file) = upload_file(
          ip_file_path = ip_file
                          ip_local     = ip_local
          ).
**                    CATCH zcx_ga_file. " Excepción clase
    cl_abap_file_utilities=>check_xstring_utf8(
    EXPORTING
      xstring           = ls_file-xstring
*         max_kb            = -1
*         all_if_7bit_ascii = abap_false
           IMPORTING
             bom               = DATA(ld_bom)
             encoding          = DATA(ld_encoding)
*         utf8_bytes_left   =
      ).
    rt_encoding = SWITCH abap_encod( ld_encoding
    WHEN cl_abap_file_utilities=>encoding_utf8 THEN '4110'   "UTF-8
    WHEN cl_abap_file_utilities=>encoding_other THEN SWITCH abap_encod(
    ld_bom
    WHEN cl_abap_file_utilities=>bom_utf16_be THEN '4102'
    WHEN cl_abap_file_utilities=>bom_utf16_le THEN '4103'
    )

    ).
*    SELECT tcpa~cpcodepage, tcpa~cpattr INTO TABLE @DATA(lt_encodings)
*      FROM tcp00a AS tcpa JOIN
*           tcp00 AS tcp ON
*           tcpa~cpcodepage = tcp~cpcodepage
*    WHERE cpattrkind = 'H'
*      AND  cpcompany IN ( 'ISO', 'SAP' ).
*    LOOP AT lt_encodings INTO DATA(ld_encoding).
*      TRY.
*          DATA(ld_string)  = cl_abap_codepage=>convert_from(
*                source      = ls_file-xstring
*                codepage    = CONV #( ld_encoding-cpattr )
*                replacement = abap_false
*          ).
*          EXIT.
*        CATCH cx_parameter_invalid_range.
*          BREAK-POINT.
*        CATCH cx_sy_codepage_converter_init.
*          BREAK-POINT.
*        CATCH cx_sy_conversion_codepage.
*          BREAK-POINT.
*        CATCH cx_parameter_invalid_type.
*          BREAK-POINT.
*
*
*      ENDTRY.
*
*    ENDLOOP.
  ENDMETHOD.


  METHOD get_encoding_local.

  ENDMETHOD.


  METHOD get_encoding_server.
    TRY.
        cl_abap_file_utilities=>check_utf8(
        EXPORTING
          file_name         = CONV #( ip_file )
          IMPORTING
              bom               = DATA(ld_bom)
              encoding          = DATA(ld_encoding)
          ).
        rt_encoding = SWITCH abap_encod( ld_encoding
          WHEN cl_abap_file_utilities=>encoding_utf8 THEN '4110'   "UTF-8
          WHEN cl_abap_file_utilities=>encoding_other THEN SWITCH abap_encod(
                            ld_bom
                            WHEN cl_abap_file_utilities=>bom_utf16_be THEN '4102'
                            WHEN cl_abap_file_utilities=>bom_utf16_le THEN '4103'
                            )

          ).
      CATCH cx_sy_file_open.
      CATCH cx_sy_file_authority.
      CATCH cx_sy_file_io.
    ENDTRY.
*    rt_encoding = switch abap_encod(  cl_abap_file_utilities=>check_for_bom( file_name =  ip_file )
*      when CL_ABAP_FILE_UTILITIES
*    ).
*CATCH cx_sy_file_open.
*CATCH cx_sy_file_authority.
*CATCH cx_sy_file_io.


  ENDMETHOD.


  METHOD get_local_home.
    CLEAR r_local_home.
    IF sy-batch IS NOT INITIAL.

      RETURN.
    ENDIF.
    IF local_home IS NOT INITIAL.
      r_local_home = local_home.
      RETURN.
    ENDIF.

    IF local_file_separator IS INITIAL.
      cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = local_file_separator
      EXCEPTIONS not_supported_by_gui = 1
        error_no_gui = 2
        cntl_error = 3
        ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.
    DATA: ld_param_value TYPE xuvalue.
    CALL FUNCTION 'ACC_USER_PARAMETER_GET'
      EXPORTING
        i_param_id    = user_parameter_local_home
      IMPORTING
        e_param_value = ld_param_value.


    IF ld_param_value IS NOT INITIAL.
*      local_home = remove_environmental_val( EXPORTING ip_path = CONV #( ld_param_value )   ).
      IF folder_is_writable( EXPORTING  ip_local = abap_true CHANGING ip_folder = ld_param_value )  .

        r_local_home = local_home = ld_param_value.
        RETURN.
      ENDIF.
    ENDIF.

    IF local_home IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
        CHANGING
          sapworkdir            = local_home
        EXCEPTIONS
          get_sapworkdir_failed = 1
          cntl_error            = 2
          error_no_gui          = 3
          not_supported_by_gui  = 4
          OTHERS                = 5.
      CALL METHOD cl_gui_cfw=>flush( ).
      IF folder_is_writable( EXPORTING ip_local = abap_true CHANGING ip_folder = local_home ).
        r_local_home = local_home.
        RETURN.
      ENDIF.
    ENDIF.
    IF local_home IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>get_temp_directory
        CHANGING
          temp_dir             = local_home
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      CALL METHOD cl_gui_cfw=>flush( ).
      IF folder_is_writable( EXPORTING ip_local = abap_true CHANGING ip_folder = local_home ).
        r_local_home = local_home.
        RETURN.
      ENDIF.
    ENDIF.
    IF local_home  IS INITIAL.
      ld_param_value  = 'C:\TEMP\'.
      IF folder_is_writable( EXPORTING ip_local = abap_true CHANGING ip_folder = ld_param_value ).
        r_local_home = local_home = ld_param_value.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD local_directory_list.

    DATA: lt_file_info TYPE TABLE OF file_info,
          ld_count     TYPE i.
    CALL METHOD cl_gui_frontend_services=>directory_list_files
      EXPORTING
        directory                   = CONV #( ip_folder )
        filter                      = CONV #( ip_filter )
        files_only                  = CONV #( ip_files_only )
        directories_only            = CONV #( ip_folders_only )
      CHANGING
        file_table                  = lt_file_info
        count                       = ld_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.
    CLEAR rt_files.
    DATA: ls_file_path TYPE ty_s_file_path.
    LOOP AT lt_file_info INTO DATA(ls_file).
      CLEAR ls_file_path.
      split_filename( EXPORTING ip_file_path = |{ ip_folder }{ ls_file-filename }|
                                ip_local = abap_true
                      CHANGING iop_path = ls_file_path ).

      IF ls_file-isdir > 0.
        ls_file_path-is_directory = abap_true.
      ENDIF.
      APPEND ls_file_path TO rt_files.
    ENDLOOP.
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

      IF syst-msgid IS NOT INITIAL AND syst-msgno IS INITIAL.
        zcx_ga_file=>raise_symsg( ).
      ELSE.
        zcx_ga_file=>raise_text( |Error at ZCL_GA_FILE=>LOCAL_FILE_EXISTS = { sy-subrc } { ip_file }| ).
      ENDIF.


    ENDIF.

  ENDMETHOD.


  METHOD local_folder_exists.
    r_exists = abap_false.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = CONV string( ip_folder )
      RECEIVING
        result               = r_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
*      zcx_ga_file=>raise_symsg( ).
      RAISE EXCEPTION TYPE zcx_ga_file
        EXPORTING
          syst_at_raise = syst.
    ENDIF.


  ENDMETHOD.


  METHOD local_folder_is_writable.
    r_is_writable = abap_false.
    IF local_folder_exists( ip_folder = ip_folder ).



      DATA: ld_file  TYPE string,
            ld_count TYPE i.
      ld_file = |{ ip_folder }dummy_file_{ ld_count }.txt|.
      WHILE local_file_exists( ip_file = ld_file ).
        ADD 1 TO ld_count.
        ld_file = |{ ip_folder }dummy_file_{ ld_count }.txt|.
      ENDWHILE.

*TRY.


      r_is_writable = download_file_to_local( ip_file_path = CONV #( ld_file )
                                              ip_file = get_dummy_file( )
                                              ip_show_messages = abap_false ).

      CHECK r_is_writable = abap_true.
      delete_file_at_local( CONV #( ld_file ) ).

*CALL METHOD zcl_ga_file=>download_file_to_local
*  EXPORTING
*    ip_file_path =
**   ip_overwrite = ABAP_FALSE
*    ip_file      =
*  RECEIVING
*    r_bool       =.
* CATCH zcx_ga_file .
*ENDTRY.


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
            IF delete_file( ip_file = ip_file ip_local = ip_local ) = abap_false.
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


  METHOD randomize_filename.
*    importing
*      value(ip_path) type string
*      value(ip_ext) type string
*      value(ip_local) type abap_bool default abap_true
*      value(ip_prefix) type string default 'rand'
*      value(ip_length) type i default 4
*    RETURNING VALUE(r_file) TYPE string.
    DATA(ld_rand) = cl_abap_random=>create(
        seed = CONV #( sy-uzeit )
    ).
    TRY.
        DO.
          DATA(ld_var) = CONV string( abs( ld_rand->int8( ) ) ).

          ld_var = condense(
                      shift_left( val = ld_var places = CONV i( strlen( ld_var ) - ip_length - 1 ) )
                   ).
          r_file = |{ ip_prefix }{ ld_var }.{ ip_ext }|.
          CHECK NOT zcl_ga_file=>file_exists(
            EXPORTING
              ip_file  = |{ format_folder( ip_local  = ip_local
                               ip_folder = ip_path
                             ) }{ r_file }|

                           ip_local = ip_local
          ).

          EXIT.
        ENDDO.
      CATCH zcx_ga_file cx_root INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_ex->get_longtext( ) ).
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
    DATA(ld_encoding) = ip_encoding.
    IF ip_encoding IS NOT SUPPLIED OR ip_encoding IS INITIAL.
      ld_encoding = get_encoding( ip_file = ip_file ip_local = ip_local ).
    ENDIF.
    IF ld_encoding IS INITIAL.
      ld_encoding = '4110'. "UTF-8 por defecto
    ENDIF.
    lo_csv_file = NEW #( encoding = ld_encoding
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
    DATA: ld_error TYPE abap_bool.
    DEFINE get_cell.
      &1->get_cell( EXPORTING ip_column = &2 ip_row = &3
      IMPORTING ep_value = &4 ).
    END-OF-DEFINITION.
    DEFINE log_error.
      ld_error = abap_true.
      sy-msgty = &1.
      sy-msgid = &2.
      sy-msgno = &3.
      sy-msgv1 = &4.
      sy-msgv2 = &5.
      sy-msgv3 = &6.
      sy-msgv4 = &7.
      IF lo_logger IS BOUND.
        lo_logger->entry_msg(  ).
      ENDIF.
      IF &8 = abap_true.
        raise_error.
      ENDIF.
    end-of-definition.
    DEFINE raise_error.
      IF ip_continue_on_error = abap_false.
      RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING syst_at_raise = sy.
      ENDIF.
    end-of-definition.
    DATA: ld_msg  TYPE sy-msgv1,
          ld_msg2 TYPE sy-msgv1,
          ld_msg3 TYPE sy-msgv1,
          ld_msg4 TYPE sy-msgv1.
    IF ip_show_error = abap_true.
      DATA(lo_logger) = NEW zcl_alog_itab_logger(  ).
    ENDIF.

    TRY.

        IF file_exists( EXPORTING ip_file = CONV #( ip_file ) ip_local = ip_local ) = abap_false.
          ld_msg = |File { ip_file } does not exist|.
          log_error  'E' '00' '398' ld_msg '' '' '' abap_true.
*          RAISE EXCEPTION TYPE zcx_ga_file
*            EXPORTING
*              error = |File { ip_file } does not exist|.
        ENDIF.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
    DATA: lo_excel  TYPE REF TO zcl_excel,
          lo_reader TYPE REF TO zif_excel_reader.
    DATA: ld_file TYPE string.
    TRY .
        IF io_xlsx_reader IS BOUND.
          lo_reader = io_xlsx_reader.
        ELSE.
          CREATE OBJECT lo_reader TYPE zcl_excel_reader_2007.
        ENDIF.
        lo_excel = lo_reader->load_file( i_filename = CONV #( ip_file )
        i_from_applserver = COND #( WHEN ip_local = abap_true THEN abap_false
                                    WHEN ip_local = abap_false THEN abap_true
                                    ELSE abap_false )
        ).


      CATCH zcx_excel.
*        RAISE EXCEPTION TYPE zcx_ga_file
*          EXPORTING
*            error = |Error reading file|.
        ld_msg = |Error reading file|.
        log_error 'E' '00' '398'  ld_msg '' '' '' abap_true.
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
          ld_row         TYPE i,
          ld_date_doubt  TYPE abap_bool.

    lt_aux_columns = lo_columns->get_components( ).
*    IF ip_wks_title IS SUPPLIED AND ip_wks_title IS NOT INITIAL.
*      lo_wks = lo_excel->get_worksheet_by_name( ip_sheet_name = ip_wks_title ).
*    ELSE.
*      lo_wks = lo_excel->get_active_worksheet( ).
*    ENDIF.
    TRY.
        lo_wks = COND #( WHEN ip_wks_title IS NOT INITIAL THEN lo_excel->get_worksheet_by_name( ip_sheet_name = CONV #( ip_wks_title ) )
                          ELSE lo_excel->get_active_worksheet( )
                        ).
      CATCH zcx_excel.

    ENDTRY.
    IF lo_wks IS NOT BOUND.

      ld_msg = COND #( WHEN ip_wks_title IS NOT INITIAL THEN |Error reading worksheet { ip_wks_title }|
                       ELSE |Error reading worksheet|
                     ).
      log_error 'E' '00' '398'  ld_msg '' '' '' abap_false.
      RETURN.
    ENDIF.
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
*      BREAK i08005.
      LOOP AT lt_columns INTO ls_column.
        ADD 1 TO ld_column.
        ASSIGN COMPONENT ls_column-name OF STRUCTURE <lf_row> TO <lf_field>.
        IF <lf_field> IS ASSIGNED.
          TRY.
              get_cell lo_wks ld_column ld_row ld_cell_value.
            CATCH zcx_excel.
*              RAISE EXCEPTION TYPE zcx_ga_file
*                EXPORTING
*                  error = |Error Reading cell { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.

              ld_msg = |Error Reading cell { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
              log_error 'E' '00' '398' ld_msg '' '' '' abap_true.
          ENDTRY.

          CASE cl_abap_datadescr=>get_data_type_kind( <lf_field> ).
            WHEN cl_abap_typedescr=>typekind_date.
              TRY.
*                  <lf_field> = zcl_excel_common=>excel_string_to_date( ip_value = ld_cell_value ).
                  IF ip_date_pattern IS SUPPLIED.
                    <lf_field> = zcl_ga_util=>check_plausability_date(
                                   EXPORTING ip_possible_date = ld_cell_value
                                             ip_date_pattern  = ip_date_pattern
                                   IMPORTING op_serious_doubt = ld_date_doubt
                                 ).
                  ELSE.
                    <lf_field> = zcl_ga_util=>check_plausability_date(
                                   EXPORTING ip_possible_date = CONV #( zcl_excel_common=>excel_string_to_date( ip_value = ld_cell_value ) )
                                             ip_date_pattern  = zcl_ga_util=>date_pattern-sap_pattern
                                   IMPORTING op_serious_doubt = ld_date_doubt
                                 ).
                  ENDIF.
                  IF ld_date_doubt = abap_true.
                    ld_msg = |Doubt at parsing { ld_cell_value } as DATE { <lf_field> ALPHA = OUT }|.
                    ld_msg2 = |CELL: { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
                    log_error 'W' '00' '398'  ld_msg '' '' '' abap_true.
                  ENDIF.
                CATCH zcx_excel zcx_ga_util
                  cx_sy_conversion_error.
*                  RAISE EXCEPTION TYPE zcx_ga_file
*                    EXPORTING
*                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS DATE|.
                  TRY.
                      <lf_field> = zcl_ga_util=>check_plausability_date(
                        EXPORTING
                          ip_possible_date = ld_cell_value
*                          ip_date_pattern  = zcl_ga_util=>date_pattern-
                        IMPORTING op_serious_doubt = ld_date_doubt
                      ).
                      IF ld_date_doubt = abap_true.
                        ld_msg = |Doubt at parsing { ld_cell_value } as DATE { <lf_field> ALPHA = OUT }|.
                        ld_msg2 = |CELL: { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
                        log_error 'W' '00' '398' ld_msg ld_msg2 '' '' abap_false.
                      ENDIF.
                    CATCH zcx_ga_util INTO DATA(lx_util). " Excepción genérica GA
                      ld_msg = lx_util->get_text( ).
                      log_error 'E' '00' '398'  ld_msg '' '' '' abap_false.
                      ld_msg = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS DATE|.
                      log_error 'E' '00' '398'  ld_msg '' '' '' abap_true.
                  ENDTRY.

              ENDTRY.
            WHEN cl_abap_typedescr=>typekind_time.
              TRY.
                  <lf_field> = zcl_excel_common=>excel_string_to_time( ip_value = ld_cell_value ).
                CATCH zcx_excel
                  cx_sy_conversion_error.
*                  RAISE EXCEPTION TYPE zcx_ga_file
*                    EXPORTING
*                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS TIME|.
                  ld_msg = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS TIME|.
                  log_error 'E' '00' '398'  ld_msg '' '' '' abap_true.
              ENDTRY.
            WHEN cl_abap_typedescr=>typekind_char OR
              cl_abap_typedescr=>typekind_csequence OR
              cl_abap_typedescr=>typekind_string. "tipo caracter
              TRY.
                  <lf_field> = ld_cell_value.
                CATCH cx_sy_conversion_error.
*                  RAISE EXCEPTION TYPE zcx_ga_file
*                    EXPORTING
*                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS string|.
                  ld_msg = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS string|.
                  log_error 'E' '00' '398'  ld_msg '' '' '' abap_true.
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
*                  <lf_field> = CONV #( ld_cell_value ).
*                  <lf_field> = CONV #( zcl_excel_common=>excel_string_to_number( CONV #( ld_cell_value ) ) ).
                  <lf_field> = CONV #( CONV f( ld_cell_value ) ).
                CATCH  cx_sy_conversion_no_number cx_sy_conversion_error zcx_excel.
*                  RAISE EXCEPTION TYPE zcx_ga_file
*                    EXPORTING
*                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS NUMBER|.
                  ld_msg = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row } AS NUMBER|.
                  log_error 'E' '00' '398'  ld_msg '' '' '' abap_true.
              ENDTRY.
            WHEN OTHERS. "tipo numero.
              TRY.
                  <lf_field> = CONV #( ld_cell_value ).
                CATCH cx_sy_conversion_error.
*                  RAISE EXCEPTION TYPE zcx_ga_file
*                    EXPORTING
*                      error = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
                  ld_msg = |Unable TO interpret { ld_cell_value } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
                  log_error 'E' '00' '398'  ld_msg '' '' '' abap_true.
              ENDTRY.
          ENDCASE.
          IF ip_apply_conv = abap_true.
            TRY.
                zcl_ga_util=>apply_conv( CHANGING iop_value = <lf_field> ).
              CATCH cx_root INTO DATA(lx_ex).
                ld_msg = |Unable TO apply conversion to { <lf_field>  } AT { zcl_excel_common=>convert_column2alpha( ld_column ) }:{ ld_row }|.
                log_error 'E' '00' '398'  ld_msg '' '' '' abap_true.
            ENDTRY.
          ENDIF.

*
        ENDIF.
      ENDLOOP.
      IF <lf_row> IS INITIAL.
        EXIT.
      ENDIF.
      APPEND <lf_row> TO <lf_table>.
      ADD 1 TO ld_row.
    ENDDO.
    IF ip_show_error = abap_true AND lo_logger IS BOUND.
      TRY.
          IF lo_logger->get_log_table( ) IS NOT INITIAL.
            lo_logger->display_as_alv_popup(
            ).
          ENDIF.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.
    CLEAR r_ok.
    IF ld_error = abap_false.
      r_ok = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD read_xlsx_as_alsm.
    DEFINE get_cell.
      &1->get_cell( EXPORTING ip_column = &2 ip_row = &3
      IMPORTING ep_value = &4 ).
    END-OF-DEFINITION.

    TRY.

        IF file_exists( EXPORTING ip_file = CONV #( ip_filename ) ip_local = ip_local ) = abap_false.
          RAISE EXCEPTION TYPE zcx_ga_file
            EXPORTING
              error = |File { ip_filename } does NOT exist|.
        ENDIF.
      CATCH zcx_ga_file INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
    DATA: lo_excel  TYPE REF TO zcl_excel,
          lo_reader TYPE REF TO zif_excel_reader.
    DATA: ld_file TYPE string.
    TRY .

        CREATE OBJECT lo_reader TYPE zcl_excel_reader_2007.
        lo_excel = lo_reader->load_file( i_filename = CONV #( ip_filename )
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
    DATA: lo_wks        TYPE REF TO zcl_excel_worksheet,
          ld_row        TYPE i,
          ld_col        TYPE i,
          ld_cell_value TYPE zexcel_cell_value.


    lo_wks = lo_excel->get_active_worksheet( ).

    ld_row = ip_begin_row.

    WHILE ld_row <= ip_end_row.
      ld_col = ip_begin_col.
      WHILE ld_col <= ip_end_col.
        CLEAR ld_cell_value.
        TRY.
            get_cell lo_wks ld_col ld_row ld_cell_value.
          CATCH zcx_excel.
            RAISE EXCEPTION TYPE zcx_ga_file
              EXPORTING
                error = |Error Reading cell { zcl_excel_common=>convert_column2alpha( ld_col ) }:{ ld_row }|.
        ENDTRY.
        IF ld_cell_value IS NOT INITIAL.
          APPEND
          VALUE #( row = ld_row
                   col = ld_col
                   value = ld_cell_value
                 ) TO rt_table.
        ENDIF.
        ADD 1 TO ld_col.
      ENDWHILE.
      ADD 1 TO ld_row.
    ENDWHILE.

  ENDMETHOD.


  METHOD remove_environmental_val.
    DATA: lt_result            TYPE match_result_tab,
          ls_result            TYPE match_result,
          ld_environmental_var TYPE string,
          ld_environmental_val TYPE string,
          ld_path              TYPE string.
    DATA: ld_file_1 TYPE string.

    IF ip_path IS INITIAL.
      RETURN.
    ENDIF.

    ld_file_1 = ld_path = ip_path.
    FIND ALL OCCURRENCES OF REGEX '%[A-Za-z0-9\(\)]*%'
    IN ld_file_1 RESULTS lt_result.

    LOOP AT lt_result INTO ls_result.
      CLEAR: ld_environmental_var, ld_environmental_val.
      ld_environmental_var =  substring( val = ld_file_1
                                         off = ls_result-offset + 1
                                         len = ls_result-length - 2 ).
      CHECK ld_environmental_var IS NOT INITIAL.

      CALL METHOD cl_gui_frontend_services=>environment_get_variable
        EXPORTING
          variable             = ld_environmental_var
        CHANGING
          value                = ld_environmental_val
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      CALL METHOD cl_gui_cfw=>flush( ).
      CHECK ld_environmental_val IS NOT INITIAL.
      ld_path = replace( val = ld_path
                        sub = |%{ ld_environmental_var }%|
                        with = ld_environmental_val ).

    ENDLOOP.
    SPLIT ld_path AT ';' INTO TABLE DATA(lt_paths).
    IF line_exists( lt_paths[ 1 ] ).
      r_path = lt_paths[ 1 ].
    ELSE.
      r_path = ld_path.
    ENDIF.
  ENDMETHOD.


  METHOD server_directory_list.
    DATA: "ld_dirname   TYPE rsmrgstr-path,
      ld_dirname   TYPE dirname_al11,
      ld_filname   TYPE rsmrgstr-name,
      ld_pattern   TYPE rsmrgstr-name,
      lt_file_list TYPE TABLE OF rsfillst,
      ls_file_list TYPE rsfillst.
    ld_dirname = ip_folder.
    ld_filname = '*'.
    ld_pattern = ip_filter.
*    CALL FUNCTION 'ZSUBST_GET_FILE_LIST'
*      EXPORTING
*        dirname      = ld_dirname
*        filenm       = ld_filname
*        pattern      = ld_pattern
*      TABLES
*        file_list    = lt_file_list
*      EXCEPTIONS
*        access_error = 1
*        OTHERS       = 2.

    DATA: sap_yes(1) VALUE 'X',
          sap_no(1)  VALUE ' '.
    DATA: no_cs VALUE ' '.

*DATA FILE like FILE_LIST.
    DATA: BEGIN OF file,
            dirname(75) TYPE c, " name of directory. (possibly truncated.)
            name(75)    TYPE c, " name of entry. (possibly truncated.)
            type(10)    TYPE c,            " type of entry.
            len(8)      TYPE p,            " length in bytes.
            owner(8)    TYPE c,            " owner of the entry.
            mtime(6)    TYPE p, " last modification date, seconds since 1970
            fmode(9)    TYPE c, " like "rwx-r-x--x": protection mode.
            useable(1)  TYPE c,
            subrc(4)    TYPE c,
            errno(3)    TYPE c,
            errmsg(40)  TYPE c,
            mod_date    TYPE d,
            mod_time(8) TYPE c,            " hh:mm:ss
            seen(1)     TYPE c,
            changed(1)  TYPE c,
          END OF file.

    DATA: errcnt(2) TYPE p VALUE 0.

    CALL 'C_DIR_READ_FINISH'             " just to be sure
        ID 'ERRNO'  FIELD ls_file_list-errno
        ID 'ERRMSG' FIELD ls_file_list-errmsg.

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD ld_dirname
                            ID 'FILE'   FIELD ld_filname
                            ID 'ERRNO'  FIELD file-errno
                            ID 'ERRMSG' FIELD file-errmsg.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.

    DO.
      CLEAR file.
      CALL 'C_DIR_READ_NEXT'
        ID 'TYPE'   FIELD file-type
        ID 'NAME'   FIELD file-name
        ID 'LEN'    FIELD file-len
        ID 'OWNER'  FIELD file-owner
        ID 'MTIME'  FIELD file-mtime
        ID 'MODE'   FIELD file-fmode
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.
      file-dirname = ld_dirname.
      MOVE sy-subrc TO file-subrc.
      CASE sy-subrc.
        WHEN 0.
          CLEAR: file-errno, file-errmsg.
          CASE file-type(1).
            WHEN 'F'.                    " normal file.
            WHEN 'f'.                    " normal file.
            WHEN OTHERS. " directory, device, fifo, socket,...
              MOVE sap_no  TO file-useable.
          ENDCASE.
          IF file-len = 0.
            MOVE sap_no TO file-useable.
          ENDIF.
        WHEN 1.
          EXIT.
        WHEN OTHERS.                     " SY-SUBRC >= 2
          ADD 1 TO errcnt.
          IF errcnt > 10.
            EXIT.
          ENDIF.
          IF sy-subrc = 5.
            MOVE: '???' TO file-type,
                  '???' TO file-owner,
                  '???' TO file-fmode.
          ELSE.
          ENDIF.
          MOVE sap_no TO file-useable.
      ENDCASE.
*   * Does the filename contains the requested pattern?
*   * Then store it, else forget it.
      IF ld_pattern = no_cs.
        MOVE-CORRESPONDING file TO ls_file_list.
        APPEND ls_file_list TO lt_file_list.
      ELSE.
        IF file-name CP ld_pattern.
          MOVE-CORRESPONDING file TO ls_file_list.
          APPEND ls_file_list TO lt_file_list..
        ENDIF.
      ENDIF.
    ENDDO.

    CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD ls_file_list-errno
        ID 'ERRMSG' FIELD ls_file_list-errmsg.
    IF sy-subrc <> 0.
    ENDIF.


    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.

    DELETE lt_file_list WHERE name = '.' OR name = '..'.
    IF ip_files_only = abap_true AND ip_folders_only = abap_false.
      DELETE lt_file_list WHERE type = 'directory'.
    ENDIF.
    IF ip_folders_only = abap_true AND ip_files_only = abap_false.
      DELETE lt_file_list WHERE type <> 'directory'.
    ENDIF.
    DATA: ls_file_path TYPE ty_s_file_path.
    LOOP AT lt_file_list INTO DATA(ls_file).
      CLEAR ls_file_path.
      split_filename( EXPORTING ip_file_path = |{ ip_folder }{ ls_file-name }|
                                ip_local = abap_false
      CHANGING iop_path = ls_file_path ).

      IF ls_file-type = 'directory'.
        ls_file_path-is_directory = abap_true.
      ENDIF.
      APPEND ls_file_path TO rt_files.
    ENDLOOP.
  ENDMETHOD.


  METHOD server_directory_list_enh.
    DATA: "ld_dirname   TYPE rsmrgstr-path,
      ld_dirname TYPE dirname_al11,
      ld_filname TYPE rsmrgstr-name,
      ld_pattern TYPE rsmrgstr-name.
    ld_dirname = ip_folder.
    ld_filname = '*'.
    ld_pattern = ip_filter.
*    CALL FUNCTION 'ZSUBST_GET_FILE_LIST'
*      EXPORTING
*        dirname      = ld_dirname
*        filenm       = ld_filname
*        pattern      = ld_pattern
*      TABLES
*        file_list    = lt_file_list
*      EXCEPTIONS
*        access_error = 1
*        OTHERS       = 2.

    DATA: sap_yes(1) VALUE 'X',
          sap_no(1)  VALUE ' '.
    DATA: no_cs VALUE ' '.

*DATA FILE like FILE_LIST.
    DATA: BEGIN OF file,
            dirname(75) TYPE c, " name of directory. (possibly truncated.)
            name(255)   TYPE c, " name of entry. (possibly truncated.)
            type(10)    TYPE c,            " type of entry.
            len(8)      TYPE p,            " length in bytes.
            owner(8)    TYPE c,            " owner of the entry.
            mtime(6)    TYPE p, " last modification date, seconds since 1970
            fmode(9)    TYPE c, " like "rwx-r-x--x": protection mode.
            useable(1)  TYPE c,
            subrc(4)    TYPE c,
            errno(3)    TYPE c,
            errmsg(40)  TYPE c,
            mod_date    TYPE d,
            mod_time(8) TYPE c,            " hh:mm:ss
            seen(1)     TYPE c,
            changed(1)  TYPE c,
          END OF file.

    DATA: lt_file_list LIKE TABLE OF file,
          ls_file_list LIKE file.
    DATA: errcnt(2) TYPE p VALUE 0.

    CALL 'C_DIR_READ_FINISH'             " just to be sure
        ID 'ERRNO'  FIELD ls_file_list-errno
        ID 'ERRMSG' FIELD ls_file_list-errmsg.

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD ld_dirname
                            ID 'FILE'   FIELD ld_filname
                            ID 'ERRNO'  FIELD file-errno
                            ID 'ERRMSG' FIELD file-errmsg.
    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.

    DO.
      CLEAR file.
      CALL 'C_DIR_READ_NEXT'
        ID 'TYPE'   FIELD file-type
        ID 'NAME'   FIELD file-name
        ID 'LEN'    FIELD file-len
        ID 'OWNER'  FIELD file-owner
        ID 'MTIME'  FIELD file-mtime
        ID 'MODE'   FIELD file-fmode
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.
      file-dirname = ld_dirname.
      MOVE sy-subrc TO file-subrc.
      CASE sy-subrc.
        WHEN 0.
          CLEAR: file-errno, file-errmsg.
          CASE file-type(1).
            WHEN 'F'.                    " normal file.
            WHEN 'f'.                    " normal file.
            WHEN OTHERS. " directory, device, fifo, socket,...
              MOVE sap_no  TO file-useable.
          ENDCASE.
          IF file-len = 0.
            MOVE sap_no TO file-useable.
          ENDIF.
        WHEN 1.
          EXIT.
        WHEN OTHERS.                     " SY-SUBRC >= 2
          ADD 1 TO errcnt.
          IF errcnt > 10.
            EXIT.
          ENDIF.
          IF sy-subrc = 5.
            MOVE: '???' TO file-type,
                  '???' TO file-owner,
                  '???' TO file-fmode.
          ELSE.
          ENDIF.
          MOVE sap_no TO file-useable.
      ENDCASE.
*   * Does the filename contains the requested pattern?
*   * Then store it, else forget it.
      IF ld_pattern = no_cs.
        MOVE-CORRESPONDING file TO ls_file_list.
        APPEND ls_file_list TO lt_file_list.
      ELSE.
        IF file-name CP ld_pattern.
          MOVE-CORRESPONDING file TO ls_file_list.
          APPEND ls_file_list TO lt_file_list..
        ENDIF.
      ENDIF.
    ENDDO.

    CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD ls_file_list-errno
        ID 'ERRMSG' FIELD ls_file_list-errmsg.
    IF sy-subrc <> 0.
    ENDIF.


    IF sy-subrc <> 0.
      zcx_ga_file=>raise_symsg( ).
    ENDIF.

    DELETE lt_file_list WHERE name = '.' OR name = '..'.
    IF ip_files_only = abap_true AND ip_folders_only = abap_false.
      DELETE lt_file_list WHERE type = 'directory'.
    ENDIF.
    IF ip_folders_only = abap_true AND ip_files_only = abap_false.
      DELETE lt_file_list WHERE type <> 'directory'.
    ENDIF.
    DATA: ls_file_path TYPE ty_s_file_path.
    LOOP AT lt_file_list INTO DATA(ls_file).
      CLEAR ls_file_path.
      split_filename( EXPORTING ip_file_path = |{ ip_folder }{ ls_file-name }|
                                ip_local = abap_false
      CHANGING iop_path = ls_file_path ).

      IF ls_file-type = 'directory'.
        ls_file_path-is_directory = abap_true.
      ENDIF.
      APPEND ls_file_path TO rt_files.
    ENDLOOP.
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
        DATA(ld_error) = lx_error->get_text( ).
        IF ld_error IS INITIAL.
          ld_error = |Error at ZCL_GA_FILE=>SERVER_FILE_EXISTS { ip_file }|.
        ENDIF.
        zcx_ga_file=>raise_text( ld_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD server_folder_exists.
    r_exists = abap_false.
    ip_folder = format_folder( EXPORTING ip_folder = ip_folder ip_local = abap_false ).
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
    DATA: filename  TYPE  string,
          extension TYPE  char20,
          message   TYPE  char100,
          ret_code  TYPE  char1.
*    CALL FUNCTION 'ZGET_FILENAME_WITH_EXT'
*      EXPORTING
*        ip_full_filename = CONV string( ip_file_path )
*      IMPORTING
*        ep_filename      = filename
*        ep_extension     = extension
*        ep_message       = message
*        ep_ret_code      = ret_code.
*
*    IF sy-subrc = 0.
*      iop_path-extension = extension.
*      iop_path-name = filename.
*    ELSE.
*      RAISE EXCEPTION TYPE zcx_ga_file
*        EXPORTING
*          syst_at_raise = syst.
*    ENDIF.
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
*      zcx_ga_file=>raise_symsg( ).
      RAISE EXCEPTION TYPE zcx_ga_file
        EXPORTING
          syst_at_raise = syst.
    ENDIF.
    "comprobamos si existe punto en la extensión, es que el nombre del archivo tiene puntos, y se ha calculado mal la extensión


    SPLIT iop_path-name_ext AT '.' INTO TABLE DATA(lt_parts).
    IF lines( lt_parts ) > 2.
      BREAK i08005.
      CLEAR: iop_path-name, iop_path-extension.
      LOOP AT lt_parts INTO DATA(ls_part).
        data(ld_part) = ls_part.
        AT FIRST.
          iop_path-name = ld_part.
          CONTINUE.
        ENDAT.
        AT LAST.
          iop_path-extension = ld_part.
          CONTINUE.
        ENDAT.

        iop_path-name = |{ iop_path-name }.{ ld_part }|.
      ENDLOOP.
    ENDIF.

*      CALL FUNCTION 'ZGET_FILENAME_WITH_EXT'
*        EXPORTING
*          ip_full_filename = iop_path-name_ext
*        IMPORTING
*          ep_filename      = iop_path-name
*          ep_extension     = iop_path-extension.
*    ENDIF.

    iop_path-path = format_folder( EXPORTING ip_folder = iop_path-path ip_local = ip_local ).
    iop_path-fullpath = ip_file_path.
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
        filename                = CONV #( ip_file_path )
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
*      zcx_ga_file=>raise_symsg( ).
      RAISE EXCEPTION TYPE zcx_ga_file
        EXPORTING
          syst_at_raise = syst.
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
*        zcx_ga_file=>raise_text( lx_error->get_text( ) ).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_error->get_text( ) ).
    ENDTRY.
    convert_x_tab( EXPORTING it_tab = lt_tab CHANGING iop_file = r_file ).
    split_filename( EXPORTING ip_file_path = ip_file_path ip_local = abap_false CHANGING iop_path = r_file-path ).

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
        CASE ip_use_abap_xlsx.
          WHEN abap_false.
            lo_file ?= NEW zcl_eui_file_io( iv_file_name = CONV #( ip_file ) ).
            lo_file->import_from_itab( ir_table = REF #( it_source_table ) ).
          WHEN abap_true.
            write_xlsx_abap(
              EXPORTING
                ip_file         = ip_file
               ip_overwrite    = ip_overwrite
               ip_local        = ip_local
               ip_start_row    = ip_start_row
               ip_start_column = ip_start_column
                it_source_table = it_source_table ).

        ENDCASE.
      CATCH zcx_ga_file INTO DATA(lx_file).
*        zcx_ga_file=>raise_text( CONV #( lx_file->get_longtext( ) ) ).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_file->get_longtext( ) ).
      CATCH zcx_excel INTO DATA(lx_excel).
*        zcx_ga_file=>raise_text( CONV #( lx_excel->get_longtext( ) ) ).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_excel->get_longtext( ) ).
      CATCH zcx_eui_exception INTO DATA(lx_eui).
*        zcx_ga_file=>raise_text( CONV #( lx_eui->get_longtext( ) ) ).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = CONV #( lx_eui->get_longtext( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD write_xlsx_abap.
    DATA: lo_excel  TYPE REF TO zcl_excel,
          lo_wsheet TYPE REF TO zcl_excel_worksheet,
          lo_writer TYPE REF TO zif_excel_writer.
    DATA: ls_table_settings TYPE zexcel_s_table_settings.
    TRY.
        CREATE OBJECT lo_excel.
        lo_wsheet = lo_excel->get_active_worksheet( ).
*    TABLE_STYLE
*    TABLE_NAME
        DATA ld_column TYPE int2.
        ld_column = ip_start_column + 1.
        ls_table_settings-top_left_column = zcl_excel_common=>convert_column2alpha( CONV #( ld_column ) ).
        ls_table_settings-top_left_row = CONV #( ip_start_row ).
        ls_table_settings-show_row_stripes = abap_false.
        ls_table_settings-show_column_stripes = abap_false.
*    BOTTOM_RIGHT_COLUMN
*    BOTTOM_RIGHT_ROW
*    NOFILTERS
        lo_wsheet->bind_table( EXPORTING
                                ip_table = it_source_table
                                is_table_settings = ls_table_settings
                               IMPORTING
                                es_table_settings = ls_table_settings ).


        CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.

        DATA ls_file TYPE ty_s_file.
        ls_file-xstring = lo_writer->write_file( lo_excel ).
        ls_file-t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring = ls_file-xstring ).
        ls_file-bytecount = xstrlen( ls_file-xstring ).
*        TRY.
        zcl_ga_file=>split_filename(
          EXPORTING
            ip_file_path = CONV #( ip_file )
            ip_local = ip_local
          CHANGING
            iop_path     = ls_file-path
        ).

        zcl_ga_file=>download_file(
          EXPORTING
            ip_file_path = ip_file
            ip_local     = ip_local
            ip_overwrite = ip_overwrite
            ip_file      = ls_file ).






      CATCH zcx_excel INTO DATA(lx_excel).
        RAISE EXCEPTION lx_excel.
      CATCH zcx_ga_file INTO DATA(lx_file).
        RAISE EXCEPTION lx_file.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
