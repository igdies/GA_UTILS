class ZCL_GA_REFLECTION_UTILS definition
  public
  final
  create public .

public section.

  class-methods GET_ATTR_VALUE
    importing
      !I_OBJECT type ref to OBJECT
      !I_ATTR type STRING
    returning
      value(R_VALUE) type STRING .
  class-methods GET_CLASS_NAME
    importing
      !I_OBJECT type ref to OBJECT
    returning
      value(R_NAME) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GA_REFLECTION_UTILS IMPLEMENTATION.


  method GET_ATTR_VALUE.
    ASSIGN i_object->(i_attr) TO FIELD-SYMBOL(<attr>).
     r_value = <attr>.
  endmethod.


  method GET_CLASS_NAME.

    data(class_desc) = cl_abap_classdescr=>describe_by_object_ref( i_object ).
    r_name = class_desc->get_relative_name( ).

  endmethod.
ENDCLASS.
