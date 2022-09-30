class ZCX_GA_UTIL definition
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
      ZCX_GA_UTIL .
  class-methods RAISE_SYMSG
    raising
      ZCX_GA_UTIL .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_GA_UTIL IMPLEMENTATION.


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


  METHOD if_message~get_longtext.
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


  METHOD if_message~get_text.
    IF me->error IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied explicitly use this
*--------------------------------------------------------------------*
      result = me->error .
    ELSEIF me->syst_at_raise IS NOT INITIAL.
*--------------------------------------------------------------------*
* If message was supplied by syst create messagetext now
*--------------------------------------------------------------------*
      MESSAGE ID syst_at_raise-msgid TYPE syst_at_raise-msgty NUMBER syst_at_raise-msgno
      WITH  syst_at_raise-msgv1 syst_at_raise-msgv2 syst_at_raise-msgv3 syst_at_raise-msgv4
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


  METHOD raise_symsg.
    RAISE EXCEPTION TYPE zcx_ga_util
      EXPORTING
        syst_at_raise = syst.
  ENDMETHOD.


  METHOD raise_text.
    RAISE EXCEPTION TYPE zcx_ga_util
      EXPORTING
        error = iv_text.
  ENDMETHOD.
ENDCLASS.
