CLASS zcl_rest_register_def DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_rest_register.
    ALIASES add_registrable FOR zif_rest_register~add_registrable.
    ALIASES collect         FOR zif_rest_register~collect.
    ALIASES do_commit       FOR zif_rest_register~do_commit.

    METHODS constructor
      IMPORTING iv_id_fuente TYPE zdserv_source_id
                iv_req_id    TYPE zdrestreqid.

  PRIVATE SECTION.
    DATA v_id_fuente TYPE zdserv_source_id.
    DATA v_req_id    TYPE zdrestreqid.

ENDCLASS.



CLASS zcl_rest_register_def IMPLEMENTATION.

  METHOD constructor.
    me->v_id_fuente = iv_id_fuente.
    me->v_req_id    = iv_req_id.
  ENDMETHOD.

  METHOD zif_rest_register~add_registrable.

  ENDMETHOD.

  METHOD zif_rest_register~collect.

  ENDMETHOD.

  METHOD zif_rest_register~do_commit.

  ENDMETHOD.

ENDCLASS.
