class ZCL_GA_LIST definition
  public
  final
  create public .

public section.

  data CURRENT_INDEX type I read-only value 0 ##NO_TEXT.

  methods APPEND_LIST
    importing
      !I_LIST type ref to ZCL_GA_LIST .
  methods DELETE
    importing
      !I_INDEX type I .
  methods INSERT
    importing
      !I_OBJECT type ref to OBJECT
      !I_INDEX type I .
  methods APPEND
    importing
      !I_OBJECT type ref to OBJECT
    returning
      value(R_KEY) type STRING .
  methods CLEAR .
  methods GET_AT
    importing
      !I_INDEX type I
    returning
      value(R_OBJECT) type ref to OBJECT .
  methods NEXT
    returning
      value(R_OBJECT) type ref to OBJECT .
  methods HAS_NEXT
    returning
      value(R_HAS_NEXT) type BOOLEAN .
  methods RESET_INDEX .
  class-methods FROM_OBJECT
    importing
      value(I_OBJECT) type ref to OBJECT
    returning
      value(R_VALUE) type STRING .
  class-methods TO_OBJECT
    importing
      !I_VALUE type ANY
    returning
      value(R_OBJECT) type ref to OBJECT .
  methods SIZE
    returning
      value(R_SIZE) type I .
protected section.

  types:
    begin of typ_entry,
            key     type string,
            object  type ref to object,
          end of typ_entry .

  data:
    the_list      type table of typ_entry .
  data:
    THE_KEYS type table of string .

  methods SET_KEY_FIELD
    importing
      !I_FIELDNAME type STRING .
private section.

  data INDEXED type BOOLEAN value ABAP_FALSE ##NO_TEXT.
  data KEYFIELD type STRING .

  methods BUILD_ENTRY
    importing
      !I_OBJECT type ref to OBJECT
    returning
      value(R_ENTRY) type TYP_ENTRY .
  methods SET_INDEX .
ENDCLASS.



CLASS ZCL_GA_LIST IMPLEMENTATION.


  method APPEND.

    data(entry) = me->build_entry( i_object ).
    append entry to me->the_list.
    collect entry-key into me->the_keys[].
    r_key = entry-key.

  endmethod.


  method APPEND_LIST.
    i_list->reset_index( ).
    while i_list->has_next( ) = abap_true.
      data(obj) = i_list->next( ).
      me->append( obj ).
    endwhile.
  endmethod.


  method BUILD_ENTRY.
      data: name     type string.

  if me->indexed = abap_true.
    data(value) = zcl_ga_reflection_utils=>get_attr_value( i_object = i_object i_attr = me->keyfield ).
    r_entry-key = value.
  endif.

  r_entry-object = i_object.
  endmethod.


  METHOD CLEAR.
    CLEAR me->the_list[].
    CLEAR me->the_keys[].
  ENDMETHOD.


  method DELETE.
    delete me->the_list index i_index.
    me->set_index( ).
  endmethod.


  method FROM_OBJECT.
        data(object) = new lcl_object( ).
    object ?= i_object.
    r_value = object->value.

*    object->value = i_value.
*    r_object = object.
  endmethod.


  method GET_AT.
    clear r_object.
    read table me->the_list index i_index into data(entry).
    r_object = entry-object.
  endmethod.


  method HAS_NEXT.
    r_has_next = abap_false.
    if me->current_index < lines( me->the_list ).
      r_has_next = abap_true.
    endif.
  endmethod.


  method INSERT.
    data(entry) = build_entry( i_object ).
    insert entry into me->the_list index i_index.
    collect entry-key into me->the_keys[].
  endmethod.


  METHOD NEXT.
   IF me->has_next( ) = abap_true.
      me->current_index = me->current_index + 1.
      r_object = me->get_at( me->current_index ).
   ENDIF.
  ENDMETHOD.


  method RESET_INDEX.
    me->current_index = 0.
  endmethod.


  method SET_INDEX.
     if me->current_index > 0.
      if lines( me->the_list ) = 0.
        me->current_index =  0.
      endif.
    endif.
  endmethod.


  method SET_KEY_FIELD.
    me->keyfield = i_fieldname.
    me->indexed = abap_true.
  endmethod.


  method SIZE.
        r_size = lines( me->the_list ).
  endmethod.


  method TO_OBJECT.
        data(object) = new lcl_object( ).
    object->value = i_value.
    r_object = object.
  endmethod.
ENDCLASS.
