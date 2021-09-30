class ZCX_REST_EX definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF zcx_serv_ex,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_serv_ex .
  constants:
    begin of BAD_JSON,
      msgid type symsgid value 'ZREST',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'P1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of BAD_JSON .
  constants:
    BEGIN OF bad_json_field,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF bad_json_field .
  constants:
    BEGIN OF no_processor,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_processor .
  constants:
    begin of INT_ERR_UNASSIGNED,
      msgid type symsgid value 'ZREST',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'P1',
      attr2 type scx_attrname value 'P2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INT_ERR_UNASSIGNED .
  constants:
    BEGIN OF int_err,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE 'P2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF int_err .
  constants:
    BEGIN OF int_err_bad_input,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF int_err_bad_input .
  constants:
    BEGIN OF bad_integer,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE 'P2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF bad_integer .
  constants:
    BEGIN OF bad_number,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE 'P2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF bad_number .
  constants:
    BEGIN OF bad_date,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE 'P2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF bad_date .
  constants:
    BEGIN OF bad_time,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE 'P2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF bad_time .
  constants:
    BEGIN OF int_err_struct_conf,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF int_err_struct_conf .
  constants:
    BEGIN OF int_err_field_conf,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE 'P2',
        attr3 TYPE scx_attrname VALUE 'P3',
        attr4 TYPE scx_attrname VALUE '',
      END OF int_err_field_conf .
  constants:
    BEGIN OF int_err_campohol,
        msgid TYPE symsgid VALUE 'ZREST',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'P1',
        attr2 TYPE scx_attrname VALUE 'P2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF int_err_campohol .
  constants:
    begin of INT_ERR_BAPI_STRUCT,
      msgid type symsgid value 'ZREST',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'P1',
      attr2 type scx_attrname value 'P2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INT_ERR_BAPI_STRUCT .
  constants:
    begin of NO_DOC_NUMBER,
      msgid type symsgid value 'ZREST',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'P1',
      attr2 type scx_attrname value 'P2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_DOC_NUMBER .
  constants:
    begin of BAD_VALUE,
      msgid type symsgid value 'ZREST',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'P1',
      attr2 type scx_attrname value 'P2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of BAD_VALUE .
  data P1 type STRING .
  data P2 type STRING .
  data P3 type STRING .
  data P4 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !P1 type STRING optional
      !P2 type STRING optional
      !P3 type STRING optional
      !P4 type STRING optional .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_REST_EX IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->p1 = p1 .
    me->p2 = p2 .
    me->p3 = p3 .
    me->p4 = p4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_serv_ex .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD if_message~get_longtext.
    IF me->previous IS BOUND.
      result = |{ me->previous->get_text( ) } => { get_text( ) }|.
    ELSE.
      result = super->get_longtext( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
