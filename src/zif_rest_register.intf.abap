INTERFACE zif_rest_register
  PUBLIC.

  "! Agrega un objeto que entrega registros
  METHODS add_registrable
    IMPORTING io_registrable TYPE REF TO zif_rest_registrable
              iv_collect     TYPE abap_bool DEFAULT abap_false
              iv_commit      TYPE abap_bool DEFAULT abap_false.

  "! Recoge los registros de los objetos registrados
  "! @parameter iv_commit | realizar el commit una vez colecte
  METHODS collect
    IMPORTING iv_commit TYPE abap_bool DEFAULT abap_false.

  "! Realiza commit de los registros hasta ahora colectados y
  "! que aún no se les da dado commit
  METHODS do_commit.

ENDINTERFACE.
