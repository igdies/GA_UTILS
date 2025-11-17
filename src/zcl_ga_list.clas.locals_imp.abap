*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

class lcl_object definition.

public section.

  data value type string.

*  methods insert
*    importing
*      !i_object type ref to object
*      !i_index type i .
protected section.
private section.

endclass.



class lcl_object implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LIST->APPEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJECT                       TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
*  method append.
*
*    append i_object to me->the_list.
*
*  endmethod.

endclass.
