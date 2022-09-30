class ZCL_GA_UTIL definition
  public
  create public .

public section.

  class-methods GET_COMPONENTS
    importing
      value(IO_STRUCTURE) type ref to CL_ABAP_STRUCTDESCR
    changing
      value(IOT_COLUMNS) type ABAP_COMPONENT_TAB .
  methods CONSTRUCTOR
    importing
      value(IP_LOGGER) type ref to ZCL_ALOG_MSG_LOGGER_BASE .
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
protected section.
private section.

  data O_LOGGER type ref to ZCL_ALOG_MSG_LOGGER_BASE .
ENDCLASS.



CLASS ZCL_GA_UTIL IMPLEMENTATION.


  METHOD constructor.
    IF ip_logger IS BOUND.
      o_logger ?= ip_logger.
    ENDIF.
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


  METHOD log_d.
    TRY.
        CHECK o_logger IS BOUND.
        o_logger->debug( iv_text ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD log_e.
    TRY.
        CHECK o_logger IS BOUND.
        o_logger->error( iv_text ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD log_exception.
    TRY.
        CHECK o_logger IS BOUND.
        o_logger->exception( ix_ex ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD log_i.
    TRY.
        CHECK o_logger IS BOUND.
        o_logger->info( iv_text ).
      CATCH cx_root.
    ENDTRY.


  ENDMETHOD.


  METHOD log_w.
    TRY.
        CHECK o_logger IS BOUND.
        o_logger->warning( iv_text ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
