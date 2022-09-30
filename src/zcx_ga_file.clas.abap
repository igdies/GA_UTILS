class ZCX_GA_FILE definition
  public
  inheriting from ZCX_GA_UTIL
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ERROR type STRING optional
      !SYST_AT_RAISE type SYST optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_GA_FILE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
ERROR = ERROR
SYST_AT_RAISE = SYST_AT_RAISE
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
