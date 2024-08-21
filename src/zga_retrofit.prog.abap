*&---------------------------------------------------------------------*
*& Report ZGA_RETROFIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zga_retrofit.
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING ip_korr   TYPE trkorr ip_down TYPE abap_bool ip_folder TYPE string OPTIONAL
                            ip_src    TYPE sysid ip_des TYPE sysid
                  ,
      run IMPORTING VALUE(ip_down) TYPE abap_bool DEFAULT abap_false
                    VALUE(ip_cpy)  TYPE abap_bool DEFAULT abap_false
                    VALUE(ip_buff) TYPE abap_bool DEFAULT abap_false
                    VALUE(ip_impt) TYPE abap_bool DEFAULT abap_false,
      output,
      check_files_at_destin RETURNING VALUE(op_bool) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_korr      TYPE trkorr,
          m_down      TYPE abap_bool,
          m_folder    TYPE string,
          m_src       TYPE sysid,
          m_des       TYPE sysid,
          o_retrofit  TYPE REF TO zcl_ga_retrofit,
          m_overwrite TYPE abap_bool,
          m_logger    TYPE REF TO zcl_alog_msg_logger_base.
    METHODS:
      download_files.
ENDCLASS.

DATA: go_app TYPE REF TO lcl_app.
PARAMETER: p_korr TYPE trkorr OBLIGATORY,
           p_src TYPE sysid,
           p_des TYPE sysid.
SELECTION-SCREEN: BEGIN OF BLOCK ret.
PARAMETER:  ck_copy AS CHECKBOX DEFAULT abap_true,
            ck_buff AS CHECKBOX DEFAULT abap_false,
            ck_impt AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN: END OF BLOCK ret.

PARAMETERS: ck_down  AS CHECKBOX DEFAULT abap_false USER-COMMAND dwn,
            p_folder TYPE string MODIF ID dwn.




AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'DWN'.
      CASE ck_down.
        WHEN abap_true.
          screen-active = 1.
          screen-invisible = 0.
        WHEN abap_false.
          screen-active = 0.
          screen-invisible = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  p_folder = zcl_ga_file=>f4_local_folder( ip_initial_folder = p_folder ).

AT SELECTION-SCREEN ON BLOCK ret.
  CHECK sy-ucomm <> 'DWN'.
  IF ck_copy = abap_true AND ( p_src IS INITIAL OR p_des IS INITIAL ).
    MESSAGE e999(zp) WITH TEXT-e02.
  ENDIF.
  IF ( ck_buff = abap_true OR ck_impt = abap_true ) AND p_des IS INITIAL.
    MESSAGE e999(zp) WITH TEXT-e03.
  ENDIF.

AT SELECTION-SCREEN ON p_folder.
  CHECK ck_down = abap_true AND sy-ucomm <> 'DWN'.
  IF NOT zcl_ga_file=>folder_is_writable( EXPORTING ip_local = abap_true CHANGING ip_folder =  p_folder  ).
    MESSAGE e999(zp) WITH TEXT-e01.
  ENDIF.

AT SELECTION-SCREEN.
  CHECK sy-ucomm <> 'DWN'.
  FREE go_app.
  go_app = NEW lcl_app( ip_korr = p_korr ip_src = p_src ip_des = p_des ip_down = ck_down ip_folder = p_folder ).


START-OF-SELECTION.
  go_app->run( EXPORTING ip_down = ck_down ip_cpy = ck_copy ip_buff = ck_buff ip_impt = ck_impt ).

END-OF-SELECTION.

  go_app->output( ).




CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    m_korr = ip_korr.
    m_src = ip_src.
    m_des = ip_des.
    m_down = ip_down.

    CASE sy-batch.
      WHEN abap_true.
        m_logger ?= NEW zcl_alog_message_logger( ).
      WHEN abap_false.
        m_logger ?= NEW zcl_alog_itab_logger( ).
    ENDCASE.
    zcl_ga_file=>get_singleton( m_logger ).
    IF m_down = abap_true.
      m_folder = ip_folder.
      IF NOT zcl_ga_file=>folder_is_writable( EXPORTING ip_local = abap_true CHANGING ip_folder = m_folder ).
        RAISE EXCEPTION TYPE zcx_ga_file
          EXPORTING
            error = |{ ip_folder } NO existe O NO tiene modo lectura.|.
      ENDIF.

    ENDIF.
    TRY.
        o_retrofit = NEW zcl_ga_retrofit( ip_order = m_korr
        ip_source_system = m_src
        ip_destin_system = m_des
        ip_logger = m_logger ).
      CATCH zcx_ga_retrofit INTO DATA(lx_error).
        m_logger->exception( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD run.
    "check ficheros en destino.
    TRY.
        IF ip_cpy = abap_true.
          IF check_files_at_destin( ) = abap_false.
            m_logger->info( |Final del proceso| ).
            RETURN.
          ENDIF.

          m_logger->info( |Inicio copiado ficheros| ).
          IF o_retrofit->copy_files( ip_overwrite = m_overwrite ) = abap_false.
            m_logger->info( |Final del proceso| ).
            RETURN.
          ELSE.
            m_logger->info( |Ficheros copiados| ).
          ENDIF.
        ENDIF.
        IF ip_buff = abap_true.
          IF o_retrofit->add_to_buffer( ) = abap_false.
            m_logger->info( |Final del proceso| ).
            RETURN.
          ELSE.
            m_logger->info( |Orden { m_korr } añadida al buffer en { m_des }| ).
          ENDIF.
        ENDIF.

        IF ip_impt = abap_true.
          IF o_retrofit->import( ) = abap_false.
            m_logger->info( |Final del proceso| ).
            RETURN.
          ELSE.
            m_logger->info( |Orden { m_korr } importada a { m_des }| ).
          ENDIF.
        ENDIF.

        IF m_down = abap_true.
          download_files( ).
        ENDIF.
        m_logger->info( |Final del proceso| ).

      CATCH zcx_ga_retrofit INTO DATA(lx_error).
        m_logger->exception( lx_error ).

    ENDTRY.
  ENDMETHOD.
  METHOD check_files_at_destin.
    DATA: ld_ans TYPE c.

    op_bool = abap_false.
    m_overwrite = abap_false.

    DATA(ld_files_exists) = o_retrofit->destin_files_exist( ).
    IF ld_files_exists = abap_true.
      m_logger->info( |Existen ficheros en destino| ).
      CALL FUNCTION 'POPUP_WITH_2_BUTTONS_TO_CHOOSE'
        EXPORTING
          defaultoption = '2'
          diagnosetext1 = 'Existen co/datafiles en destino'
          textline1     = '¿Desea sobreescribir los ficheros?'
          text_option1  = 'Sí'
          text_option2  = 'No'
          titel         = '¡Cuidado!'
        IMPORTING
          answer        = ld_ans.

      CASE ld_ans.
        WHEN '1'.
          op_bool = abap_true.
          m_overwrite = abap_true.
          m_logger->info( |Se sobreescribirán| ).
          RETURN.
        WHEN '2'.
          op_bool = abap_false.
          m_overwrite = abap_false.
          m_logger->info( |No se sobreescribirán| ).
          RETURN.
      ENDCASE.

    ELSE.
      op_bool = abap_true.
      m_overwrite = abap_false.
    ENDIF.

  ENDMETHOD.
  METHOD output.
    IF sy-batch IS INITIAL.
      TRY.
          CAST zcl_alog_itab_logger( m_logger )->display_as_alv( ).
        CATCH cx_root INTO DATA(lx_error).
          MESSAGE e999(zp) WITH 'Error al mostrar resultado' lx_error->get_longtext( ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD download_files.
    TRY.
        "cofile
        CASE zcl_ga_file=>copy_file(
        EXPORTING
          ip_source      = |{ o_retrofit->get_source_system( )-folder }\\cofiles\\{ o_retrofit->get_cofile( ) }|
          ip_source_local = abap_false
          ip_destination = |{ m_folder }\\{ o_retrofit->get_cofile( ) }|
          ip_destination_local = abap_true
          ip_overwrite   = abap_true
          ).
          WHEN abap_true.
            m_logger->info( |Fichero cofile { o_retrofit->get_cofile( ) } copiado | ).
            "datafiles
            CASE zcl_ga_file=>copy_file(
                EXPORTING
                  ip_source      = |{ o_retrofit->get_source_system( )-folder }\\data\\{ o_retrofit->get_datafile( ) }|
                  ip_source_local = abap_false
                  ip_destination = |{ m_folder }\\{ o_retrofit->get_datafile( ) }|
                  ip_destination_local = abap_true
                  ip_overwrite   = abap_true
                  ).
              WHEN abap_true.
                m_logger->info( |Fichero dafile { o_retrofit->get_datafile( ) } copiado | ).

              WHEN abap_false.
                m_logger->error( |Error al copiar fichero datafile { o_retrofit->get_datafile( ) }| ).

            ENDCASE.
          WHEN abap_false.
            m_logger->error( |Error al copiar fichero cofile { o_retrofit->get_cofile( ) }| ).


        ENDCASE.


      CATCH zcx_ga_file INTO DATA(lx_file).
        m_logger->exception( lx_file ).
        m_logger->error( lx_file->get_text( ) ).

    ENDTRY.
  ENDMETHOD.
ENDCLASS.
