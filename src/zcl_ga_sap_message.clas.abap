class ZCL_GA_SAP_MESSAGE definition
  public
  final
  create public .

public section.

  data TYPE type BAPI_MTYPE read-only .
  data ID type SYMSGID .
  data NUMBER type SYMSGNO .
  data MESSAGE type BAPI_MSG .
  data ARG1 type SYMSGV .
  data ARG2 type SYMSGV .
  data ARG3 type SYMSGV .
  data ARG4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !I_TYPE type BAPI_MTYPE .
  class-methods FROM_BAPIRET2
    importing
      !I_BAPIRET2 type BAPIRET2
    returning
      value(R_MESSAGE) type ref to ZCL_GA_SAP_MESSAGE .
  methods GET_MESSAGE
    returning
      value(R_MESSAGE) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GA_SAP_MESSAGE IMPLEMENTATION.


  method CONSTRUCTOR.
     me->type = i_type.
  endmethod.


  method FROM_BAPIRET2.
        r_message = new zcl_ga_sap_message( i_bapiret2-type ).
    r_message->id = i_bapiret2-id.
    r_message->number = i_bapiret2-number.
    r_message->message = i_bapiret2-message.
    r_message->arg1 = i_bapiret2-message_v1.
    r_message->arg2 = i_bapiret2-message_v2.
    r_message->arg3 = i_bapiret2-message_v3.
    r_message->arg4 = i_bapiret2-message_v4.
  endmethod.


  method GET_MESSAGE.
        if me->message is initial.
      me->message = zcl_ga_io_utils=>get_std_message( me ).
    endif.
    r_message = me->message.
  endmethod.
ENDCLASS.
