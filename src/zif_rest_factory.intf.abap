INTERFACE zif_rest_factory
  PUBLIC.

  METHODS init_validator
    IMPORTING iv_oper       TYPE zdrestoperacion
              iv_id_fuente  TYPE zdserv_source_id
    RETURNING VALUE(ro_val) TYPE REF TO zif_rest_validator.

  METHODS init_finder
    IMPORTING iv_oper           TYPE zdrestoperacion
              iv_id_fuente      TYPE zdserv_source_id
    RETURNING VALUE(ro_finder) TYPE REF TO zif_rest_finder.

  METHODS init_creator
    IMPORTING iv_oper           TYPE zdrestoperacion
              iv_id_fuente      TYPE zdserv_source_id
    RETURNING VALUE(ro_creator) TYPE REF TO zif_rest_creator.

ENDINTERFACE.
