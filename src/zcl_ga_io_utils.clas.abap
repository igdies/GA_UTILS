CLASS zcl_ga_io_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_std_message
      IMPORTING
        !i_message    TYPE REF TO zcl_ga_sap_message
      RETURNING
        VALUE(r_text) TYPE string .
    CLASS-METHODS get_sap_language
      IMPORTING
        !i_iso         TYPE string
      RETURNING
        VALUE(r_spras) TYPE spras .
    CLASS-METHODS get_iso_language
      IMPORTING
        !i_spras     TYPE spras
      RETURNING
        VALUE(r_iso) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ga_io_utils IMPLEMENTATION.


  METHOD get_iso_language.
    CASE i_spras.
      WHEN 'S'.     r_iso = 'es_ES'.
      WHEN 'E'.     r_iso = 'en_GB'.
      WHEN 'F'.     r_iso = 'fr_FR'.
      WHEN 'P'.     r_iso = 'pt_PT'.
      WHEN 'D'.     r_iso = 'de_DE'.
      WHEN OTHERS.  r_iso = 'en_US'.
    ENDCASE.
  ENDMETHOD.


  METHOD get_sap_language.
    DATA: lang        TYPE c LENGTH 2.

    DATA(iso) = i_iso.
    TRANSLATE iso TO LOWER CASE.
    IF iso = 'en_us'.
      lang = '6N'.
    ELSE.
      DATA(l) = strlen( iso ).
      IF l >= 2.
        lang = iso+0(2).
      ENDIF.
    ENDIF.

    IF lang IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          input  = lang
        IMPORTING
          output = r_spras.
    ELSE.
      r_spras = sy-langu.
    ENDIF.

  ENDMETHOD.


  METHOD get_std_message.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = i_message->id
        msgnr               = i_message->number
        msgv1               = i_message->arg1
        msgv2               = i_message->arg2
        msgv3               = i_message->arg3
        msgv4               = i_message->arg4
      IMPORTING
        message_text_output = r_text.


  ENDMETHOD.
ENDCLASS.
