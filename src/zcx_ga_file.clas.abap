class ZCX_GA_FILE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  data ERROR type STRING .
  data SYST_AT_RAISE type SYST .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ERROR type STRING optional
      !SYST_AT_RAISE type SYST optional .
  class-methods RAISE_TEXT
    importing
      value(IV_TEXT) type CLIKE
    raising
      ZCX_GA_FILE .
  class-methods RAISE_SYMSG
    raising
      ZCX_GA_FILE .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_GA_FILE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->ERROR = ERROR .
me->SYST_AT_RAISE = SYST_AT_RAISE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD IF_MESSAGE~GET_LONGTEXT.
    IF   me->error         IS NOT INITIAL
    OR me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this as longtext as well
*--------------------------------------------------------------------*
      result = me->get_text( ).
    ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
      result = super->if_message~get_longtext( preserve_newlines = preserve_newlines ).
    ENDIF.
  ENDMETHOD.


  METHOD IF_MESSAGE~GET_TEXT.
    IF me->error IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this
*--------------------------------------------------------------------*
      result = me->error .
    ELSEIF me->syst_at_raise IS NOT INITIAL
      AND me->syst_at_raise-msgid IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied by syst create messagetext now
*--------------------------------------------------------------------*
      DATA(ld_msgty) =
           SWITCH syst_msgty( me->syst_at_raise-msgty
             WHEN 'A' OR 'E' OR 'I' OR 'W' OR 'S' OR 'X'
               THEN me->syst_at_raise-msgty
             ELSE 'E'

           ).
      MESSAGE ID me->syst_at_raise-msgid TYPE ld_msgty
      NUMBER me->syst_at_raise-msgno
      WITH  me->syst_at_raise-msgv1
            me->syst_at_raise-msgv2
            me->syst_at_raise-msgv3
            me->syst_at_raise-msgv4
      INTO  result.
    ELSE.
*--------------------------------------------------------------------*
* otherwise use standard method to derive text
*--------------------------------------------------------------------*
      CALL METHOD super->if_message~get_text
        RECEIVING
          result = result.
    ENDIF.
  ENDMETHOD.


  METHOD RAISE_SYMSG.
    RAISE EXCEPTION TYPE zcx_ga_file
      EXPORTING
        syst_at_raise = syst.
  ENDMETHOD.


  METHOD RAISE_TEXT.
    RAISE EXCEPTION TYPE zcx_ga_file
      EXPORTING
        error = iv_text.
  ENDMETHOD.
ENDCLASS.
