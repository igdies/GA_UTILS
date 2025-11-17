class ZCL_GA_FILE_CSV definition
  public
  inheriting from ZCL_WD_CSV_FILE
  create public .

public section.

  constants C_DELIMITER_EMPTY type TY_DELIMITER value '' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !ENCODING type ABAP_ENCOD default '4110'
      !REPLACEMENT type ABAP_REPL default '#'
      !IGNORE_CERR type ABAP_BOOL default ABAP_TRUE
      !ENDOFLINE type CSEQUENCE default ZCL_WD_CSV=>C_ENDOFLINE_CR_LF
      !SEPARATOR type TY_SEPARATOR default ZCL_WD_CSV=>C_SEPARATOR_TAB
      !DELIMITER type TY_DELIMITER default ZCL_WD_CSV=>C_DELIMITER_DOUBLE_QUOTE
      !CONV_EXIT type ABAP_BOOL default ABAP_FALSE
      !TRIM_SPACES type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_WD_CSV_INVALID_ENDOFLINE
      ZCX_WD_CSV_INVALID_SEPARATOR
      ZCX_WD_CSV_INVALID_DELIMITER .

  methods SET_DELIMITER
    redefinition .
protected section.

  methods GENERATE_CELL
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_GA_FILE_CSV IMPLEMENTATION.


  METHOD constructor.

    super->constructor(
      EXPORTING
        encoding    = encoding
        replacement = replacement
        ignore_cerr = ignore_cerr
        endofline   = endofline
        separator   = separator
*            delimiter   = delimiter
        conv_exit   = conv_exit
        trim_spaces = trim_spaces
    ).
    IF delimiter IS SUPPLIED.
      TRY.
          set_delimiter(
              delimiter = delimiter
          ).
        CATCH zcx_wd_csv_invalid_delimiter INTO DATA(lo_ex_del).
          RAISE EXCEPTION lo_ex_del.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD generate_cell.
    IF delimiter <> c_delimiter_empty.
      result = super->generate_cell(
        EXPORTING
          fieldname = fieldname
          fieldtype = fieldtype
          value     = value
      ).
    ELSE.
* ---------------------------------------------------------------------
      DATA:
        cell    TYPE c LENGTH 200, " randomly selected, should be enough right?
        delimit TYPE abap_bool.

* ---------------------------------------------------------------------
      CASE conv_exit.
        WHEN abap_true.
          WRITE value TO cell LEFT-JUSTIFIED.
          result = cell.
        WHEN abap_false.
          result = value.
      ENDCASE.

* ---------------------------------------------------------------------
      IF trim_spaces_enabled = abap_true.
        CONDENSE result.
      ENDIF.

* ---------------------------------------------------------------------
      " escape quotes
*      IF find( val = result sub = delimiter ) >= 0.
*        delimit = abap_true.
*        result = replace( val  = result
*                          sub  = delimiter
*                          occ  = 0
*                          with = delimiter && delimiter ).
*      ENDIF.

* ---------------------------------------------------------------------
      " if the cell contains a separator or any newline character, it needs to be delimited
*      IF delimit = abap_false
*      AND (    find( val = result sub = separator                          ) >= 0
*            OR find( val = result sub = cl_abap_char_utilities=>cr_lf      ) >= 0
*            OR find( val = result sub = cl_abap_char_utilities=>cr_lf+0(1) ) >= 0
*            OR find( val = result sub = cl_abap_char_utilities=>cr_lf+1(1) ) >= 0 ).
*        delimit = abap_true.
*      ENDIF.

* ---------------------------------------------------------------------
*      IF delimit = abap_true.
*        result = delimiter && result && delimiter.
*      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD set_delimiter.
    IF delimiter IS INITIAL OR delimiter = space.
      me->delimiter = delimiter.
    ELSE.
      TRY.
          super->set_delimiter( delimiter = delimiter ).
        CATCH zcx_wd_csv_invalid_delimiter .
          RAISE EXCEPTION TYPE zcx_wd_csv_invalid_delimiter
            EXPORTING
              delimiter = delimiter.
      ENDTRY.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
