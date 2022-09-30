*&---------------------------------------------------------------------*
*& Report ZGA_RETROFIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zga_retrofit.
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING ip_korr TYPE trkorr ip_src TYPE sysid ip_des TYPE sysid,
      run IMPORTING VALUE(ip_cpy)  TYPE abap_bool DEFAULT abap_false
                    VALUE(ip_buff) TYPE abap_bool DEFAULT abap_false
                    VALUE(ip_impt) TYPE abap_bool DEFAULT abap_false,
      output,
      check_files_at_destin RETURNING VALUE(op_bool) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_korr      TYPE trkorr,
          m_src       TYPE sysid,
          m_des       TYPE sysid,
          o_retrofit  TYPE REF TO zcl_ga_retrofit,
          m_overwrite TYPE abap_bool,
          m_logger    TYPE REF TO zcl_alog_msg_logger_base.
ENDCLASS.

DATA: go_app TYPE REF TO lcl_app.
PARAMETER: p_korr TYPE trkorr,
           p_src TYPE sysid,
           p_des TYPE sysid.
PARAMETER: ck_copy AS CHECKBOX DEFAULT abap_true,
           ck_buff AS CHECKBOX DEFAULT abap_false,
           ck_impt AS CHECKBOX DEFAULT abap_false.




AT SELECTION-SCREEN.
  FREE go_app.
  go_app = NEW lcl_app( ip_korr = p_korr ip_src = p_src ip_des = p_des ).


START-OF-SELECTION.
  go_app->run( EXPORTING ip_cpy = ck_copy ip_buff = ck_buff ip_impt = ck_impt ).

END-OF-SELECTION.

  go_app->output( ).




CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    m_korr = ip_korr.
    m_src = ip_src.
    m_des = ip_des.
    CASE sy-batch.
      WHEN abap_true.
        m_logger ?= NEW zcl_alog_message_logger( ).
      WHEN abap_false.
        m_logger ?= NEW zcl_alog_itab_logger( ).
    ENDCASE.
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
ENDCLASS.
