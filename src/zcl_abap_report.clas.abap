class ZCL_ABAP_REPORT definition
  public
  create public .

*"* public components of class ZCL_ABAP_REPORT
*"* do not include other source files here!!!
public section.

  types:
    TPARAMS type table of RSPARAMS .

  methods ADD_PARAM
    importing
      value(SELNAME) type RSSCR_NAME
      value(KIND) type RSSCR_KIND default 'P'
      value(SIGN) type TVARV_SIGN default 'I'
      value(OPTION) type TVARV_OPTI default 'EQ'
      value(LOW) type ANY
      value(HIGH) type ANY optional .
  methods ADD_RANGE
    importing
      value(SELNAME) type RSSCR_NAME
      value(IR_RANGE) type STANDARD TABLE .
  methods RUN_REPORT_AND_GET_ALV_DATA
    importing
      value(REPORT_NAME) type REPID
    changing
      value(IO_REF_DATA) type ref to DATA optional
      value(TABLE) type TABLE optional .
  methods CONSTRUCTOR .
  methods FILL_SELECTION_TABLE
    importing
      !TPARAM type TPARAMS .
  methods RUN_REPORT
    importing
      !REPORT_NAME type TRDIR-NAME .
  methods RUN_REPORT2
    importing
      !REPORT_NAME type TRDIR-NAME
      !VARIAN_NAME type VARID-VARIANT .
  methods RUN_REPORT3
    importing
      !REPORT_NAME type TRDIR-NAME .
  methods GET_HTML
    exporting
      !HTML type STRING .
  methods GET_STATS
    exporting
      !STATS type STRING .
  methods GET_RESULTS
    importing
      !CADENA type STRING optional
    exporting
      !NRESULTS type STRING .
protected section.
*"* protected components of class ZCL_ABAP_REPORT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ABAP_REPORT
*"* do not include other source files here!!!

  data XML type STRING .
  data:
    tparam_priv TYPE TABLE OF rsparams .
  data XMLSTAT type STRING .

  methods FORMAT_HTML
    changing
      value(XML) type STRING
      value(XMLSTAT) type STRING .
  methods REMOVE_TAG
    importing
      value(TAG) type STRING
    changing
      value(HTML_TABLE) type HTML_TABLE .
ENDCLASS.



CLASS ZCL_ABAP_REPORT IMPLEMENTATION.


METHOD add_param.
  DATA lparam TYPE rsparams.
  lparam-selname = selname.
  TRANSLATE lparam-selname TO UPPER CASE.
  lparam-kind = kind.
  lparam-sign    = sign.
  lparam-option  = option.
  lparam-low     = low.
  IF high IS SUPPLIED.
    lparam-high    = high.
  ENDIF.
  APPEND lparam TO tparam_priv.
ENDMETHOD.


  METHOD add_range.
    DATA lparam TYPE rsparams.
    LOOP AT ir_range ASSIGNING FIELD-SYMBOL(<lf_range>).
      MOVE-CORRESPONDING <lf_range> TO lparam.
      lparam-selname = to_upper( val = selname ).
      lparam-kind = 'S'.
      APPEND lparam TO tparam_priv.
    ENDLOOP.
  ENDMETHOD.


method CONSTRUCTOR.
endmethod.


method FILL_SELECTION_TABLE.
   tparam_priv = tparam.
endmethod.


METHOD format_html.
  DATA: lt_htmltab type table of w3html, ls_htmlline type w3html.
    DATA: lt_listobject type table of abaplist, ls_listobject type abaplist.
  DATA: len TYPE i,
        htmlstr TYPE string,
        htmlline TYPE w3html-line,
        strline  TYPE string.

*  -- output the ABAP list into an HTML list
  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = lt_listobject
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

*  -- transform the list to HTML
  CALL FUNCTION 'WWW_HTML_FROM_LISTOBJECT'
    EXPORTING
      report_name   = space
      template_name = 'WEBREPORTING_REPORT'
    TABLES
      html          = lt_htmltab
      listobject    = lt_listobject.
*        ENDIF.
  DATA: fromtabix TYPE i.
  DESCRIBE TABLE lt_htmltab LINES len.
  READ TABLE lt_htmltab into ls_htmlline INDEX 1.
  WHILE NOT ls_htmlline CS '<SCRIPT'.
    ADD 1 TO fromtabix.
    READ TABLE lt_htmltab into ls_htmlline INDEX fromtabix.
    IF fromtabix = len.
      EXIT.
    ENDIF.
  ENDWHILE.

  LOOP AT lt_htmltab into ls_htmlline FROM fromtabix.
    IF ls_htmlline CS '</script>'.
      DELETE lt_htmltab INDEX sy-tabix.
      EXIT.
    ELSE.
      DELETE lt_htmltab INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

*    IF EXPA IS INITIAL.

* remove tags - particular conditions

  remove_tag( EXPORTING tag = '<blockquote>' CHANGING html_table = lt_htmltab ).
  remove_tag( EXPORTING tag = '</blockquote>' CHANGING html_table = lt_htmltab ).
  remove_tag( EXPORTING tag = '<!script!>' CHANGING html_table = lt_htmltab ).


  LOOP AT  lt_htmltab into ls_htmlline.
*        if htmltab-line cs '<html'.
*          htmltab-line = '<html  dir="ltr"><head>'.
*          modify htmltab index sy-tabix.
*        endif.
    IF ls_htmlline-line CS '<body'.
      ls_htmlline-line = '<body topmargin="0" leftmargin="0">'.
      MODIFY lt_htmltab from ls_htmlline INDEX sy-tabix.
    ENDIF.


    REPLACE '&#62;' WITH '  >  ' INTO ls_htmlline-line.

*      modify table class

* miguel 03/07/04 - remove any style from the page
    IF ls_htmlline-line CS 'border-collapse'.
      CONCATENATE '<style>'
      'body  { font-family: Arial;'
*           'font-size:10px'
      ' }'
      'table {border-collapse: collapse; font-family:Arial;'
      'font-size:10px font-color:black}</style>'
      INTO ls_htmlline-line SEPARATED BY space.
      MODIFY lt_htmltab from ls_htmlline INDEX sy-tabix.
    ENDIF.

* add error control to avoid space erase
    DATA: length TYPE i.
    IF ls_htmlline-line+254 = space.
      ls_htmlline-line+254 = '`'.
      CONCATENATE xml ls_htmlline-line '@`' INTO xml.
    ELSE.
      CONCATENATE xml ls_htmlline-line INTO xml.
    ENDIF.
  ENDLOOP.
  WHILE xml CS '`@`'.
    REPLACE '`@`' WITH space INTO xml.
  ENDWHILE.

* get the statistics
  xmlstat = xml.
*    shift xmlstat up to '</pre></pre>'.
  SHIFT xmlstat UP TO '<table  class='.
  len = STRLEN( xmlstat ).
*    shift xmlstat by 12 places.

* get report content
*    shift xml right by len places.
  len = STRLEN( xml ) - len.
  xml = xml(len).
  CONCATENATE xml '</body></html>' INTO xml.
  CONCATENATE '<report><![CDATA[' xml ']]></report>'
  INTO xml.
ENDMETHOD.


method GET_HTML.
  format_html( changing xml = html xmlstat = xmlstat ).
    xml = html.
endmethod.


method GET_RESULTS.
  DATA: str    TYPE string,
        strcmp TYPE string.


  IF cadena IS INITIAL.
    strcmp = 'Números de personal impresos......: '.
  ELSE.
    strcmp = cadena.
  ENDIF.
  str = xml.
  IF str CS strcmp.
    SHIFT str BY sy-fdpos PLACES.
    SHIFT str BY 36 PLACES.
    nresults = str(3).
  ENDIF.

endmethod.


method GET_STATS.
      concatenate '<reportStats><![CDATA[' xmlstat ']]></reportStats>'
into stats.
endmethod.


method REMOVE_TAG.
  DATA: BEGIN OF s_tab,
    line1 type w3html-LINE,
    line2 type w3html-LINE,
  END OF s_tab.
  data: ls_html_line type line of html_table.
  DATA: I   TYPE I,
        j   TYPE I,
        lin TYPE I,
        len TYPE I,
        beginpos type sy-fdpos,
        endpos   type sy-fdpos,
        str_replace(200).


**str_replace = p_tag.
  len = STRLEN( tag ).
*while i < len.
*  concatenate str_replace '*' into str_replace.
*  add 1 to i.
*endwhile.
*condense str_replace.
*translate str_replace using '* '.
*len = strlen( str_replace ).

  DESCRIBE TABLE html_table LINES lin.
  I = 1.

  READ TABLE html_table INDEX I INTO s_tab-line1.
  ADD 1 TO I.

  WHILE I <= lin.
    READ TABLE html_table INDEX I INTO s_tab-line2.


    WHILE s_tab CS tag.
* search position of string
      SEARCH s_tab FOR tag.
      IF sy-subrc = 0.
        beginpos = sy-fdpos.
        endpos = sy-fdpos + len.
        WHILE beginpos < endpos.
          s_tab+beginpos(1) = space.
          ADD 1 TO beginpos.
        ENDWHILE.
      ENDIF.
    ENDWHILE.
* update previous line
    j = I - 1.
    READ TABLE html_table into ls_html_line INDEX j.
    ls_html_line-LINE = s_tab-line1.
    MODIFY html_table from ls_html_line INDEX j.
* /update
* move line2 into line1
    s_tab-line1 = s_tab-line2.
    CLEAR s_tab-line2.

    ADD 1 TO I.
  ENDWHILE.
* update last line
  I = I - 1.
  ls_html_line-LINE = s_tab-line2.
  MODIFY html_table from ls_html_line INDEX I.
endmethod.


method RUN_REPORT.
  SUBMIT (report_name)
  WITH selection-TABLE tparam_priv
  EXPORTING list TO MEMORY
    AND RETURN.
endmethod.


method RUN_REPORT2.
  SUBMIT (report_name)  USING selection-SET varian_name
        WITH selection-TABLE tparam_priv
  EXPORTING list TO MEMORY
    AND RETURN.
endmethod.


method RUN_REPORT3.
  submit (report_name)  with selection-table tparam_priv
                        and return.
endmethod.


METHOD run_report_and_get_alv_data.
  DATA          lr_data               TYPE REF TO data.
  DATA          lr_data_descr          TYPE REF TO cl_abap_datadescr.
  FIELD-SYMBOLS <lt_data>             TYPE ANY TABLE.
  cl_salv_bs_runtime_info=>set(
  EXPORTING display  = abap_false
    metadata = abap_false
  data     = abap_true ).
  SUBMIT (report_name)
  WITH SELECTION-TABLE tparam_priv
  EXPORTING LIST TO MEMORY
    AND RETURN.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data_descr      = lr_data_descr ).
      IF lr_data_descr IS BOUND.
        CREATE DATA lr_data           TYPE HANDLE lr_data_descr.

        ASSIGN lr_data->*           TO <lt_data>.

        cl_salv_bs_runtime_info=>get_data(
        IMPORTING
          t_data      = <lt_data>
          ).
        IF table IS SUPPLIED.
          table = <lt_data>.
        ENDIF.
        IF io_ref_data IS SUPPLIED.
          io_ref_data = lr_data.
        ENDIF.
      ENDIF.
    CATCH cx_salv_bs_sc_runtime_info.
      MESSAGE `Unable to retrieve ALV data` TYPE 'E'.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

ENDMETHOD.
ENDCLASS.
