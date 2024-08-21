CLASS zcl_ga_html DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      create_element IMPORTING ip_tag TYPE string,   " new element
      add_attribute IMPORTING ip_attr TYPE string    " add any attribute
                              ip_val  TYPE string,
      add_inner_html IMPORTING ip_val TYPE string,   " Inner text
      append_child                                " Add a child
        IMPORTING io_tag TYPE REF TO zcl_ga_html,
      get_html
        RETURNING VALUE(rv_string) TYPE string.   " Get HTML string
  PROTECTED SECTION.
    DATA: tag TYPE string.
    DATA: inner_html TYPE string.
    TYPES:
      BEGIN OF ty_attr,
        attr TYPE string,
        val  TYPE string,
      END OF ty_attr.
    DATA: attrs TYPE STANDARD TABLE OF ty_attr.
    DATA: children TYPE STANDARD TABLE OF REF TO zcl_ga_html.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ga_html IMPLEMENTATION.
  METHOD add_attribute.
    DATA: ls_attr LIKE LINE OF me->attrs.
    ls_attr-attr = ip_attr.
    ls_attr-val  = ip_val.
    APPEND ls_attr TO me->attrs.
  ENDMETHOD.

  METHOD add_inner_html.
    me->inner_html = ip_val.
  ENDMETHOD.

  METHOD append_child.
    APPEND io_tag TO me->children.
  ENDMETHOD.

  METHOD create_element.
    me->tag = tag.
  ENDMETHOD.

  METHOD get_html.
    DATA: lv_string TYPE string.
    DATA: lv_attr_val TYPE string.
    DATA: ls_attr LIKE LINE OF me->attrs.
    DATA: lo_child TYPE REF TO zcl_ga_html.
    DATA: lv_child_html TYPE string.

*   opening bracket
    CONCATENATE `<` me->tag INTO lv_string.
    LOOP AT me->attrs INTO ls_attr.
      CONCATENATE ls_attr-attr `='` ls_attr-val `'`
        INTO lv_attr_val.
      CONCATENATE lv_string lv_attr_val
        INTO lv_string SEPARATED BY space.
    ENDLOOP.
    CONCATENATE lv_string `>` INTO lv_string.

*   inner html
    CONCATENATE lv_string me->inner_html INTO lv_string.

*   child
    LOOP AT me->children INTO lo_child.
      lv_child_html = lo_child->get_html( ).
      CONCATENATE lv_string lv_child_html
        INTO lv_string.
      CLEAR lv_child_html.
    ENDLOOP.

*   closing
    CONCATENATE lv_string `</` me->tag `>`
     INTO lv_string.

*   back the HTML
    rv_string = lv_string.
  ENDMETHOD.

ENDCLASS.
