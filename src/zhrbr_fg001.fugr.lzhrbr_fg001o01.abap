*----------------------------------------------------------------------*
***INCLUDE LZHRIC_FG001O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  PERFORM f_get_word.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
FORM f_get_word.

  CLASS c_oi_errors DEFINITION LOAD.
  CLASS cl_gui_cfw DEFINITION LOAD.
  DATA: container TYPE REF TO cl_gui_custom_container.
  DATA: item_url(256).
  DATA: control TYPE REF TO i_oi_container_control.
  DATA: link_server_decl TYPE REF TO i_oi_link_server.
  DATA: retcode       TYPE soi_ret_string,
        document_type TYPE soi_document_type
*                          VALUE soi_doctype_excel97_sheet.
                          VALUE soi_doctype_word97_document.
  DATA: proxy TYPE REF TO i_oi_document_proxy.
  DATA: bds_instance     TYPE REF TO cl_bds_document_set,
        doc_uris         TYPE sbdst_uri,
        wa_doc_uris      LIKE LINE OF doc_uris,
        doc_components   TYPE sbdst_components,
        doc_signature    TYPE sbdst_signature,
        wa_doc_signature LIKE LINE OF doc_signature.


  retcode = c_oi_errors=>ret_ok.
  IF control IS INITIAL.
    DATA: b_has_activex.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = b_has_activex.
    IF b_has_activex IS INITIAL.
      MESSAGE 'No Windows GUI' TYPE 'I'.EXIT.
    ENDIF.

    CALL METHOD c_oi_container_control_creator=>get_container_control
      IMPORTING
        control = control
        retcode = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

    CREATE OBJECT container
      EXPORTING
        container_name = 'CONTAINER'.

    CALL METHOD control->init_control
      EXPORTING
        r3_application_name      = 'Demo'
        inplace_enabled          = ''
        inplace_scroll_documents = 'X'
        parent                   = container
        register_on_close_event  = 'X'
        register_on_custom_event = 'X'
      IMPORTING
        retcode                  = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

    CALL METHOD control->get_link_server
      IMPORTING
        link_server = link_server_decl
        retcode     = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

    CALL METHOD link_server_decl->start_link_server
      IMPORTING
        retcode = retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

* Fill the template
    IF NOT control IS INITIAL.
      CALL METHOD control->get_document_proxy
        EXPORTING
          document_type  = document_type
        IMPORTING
          document_proxy = proxy
          retcode        = retcode.
      IF bds_instance IS INITIAL.
        CREATE OBJECT bds_instance.
      ENDIF.
      CALL METHOD bds_instance->get_with_url
        EXPORTING
          classname  = gv-classname
          classtype  = gv-classtype
          object_key = gv-object_key
        CHANGING
          uris       = doc_uris
          signature  = doc_signature.
      READ TABLE doc_uris INTO wa_doc_uris INDEX 1.
      item_url = wa_doc_uris-uri.
      IF NOT bds_instance IS INITIAL.
        FREE bds_instance.
      ENDIF.
      CALL METHOD proxy->open_document
        EXPORTING
          open_inplace = 'X'
          document_url = item_url.
      CALL METHOD proxy->update_document_links
        IMPORTING
          retcode = retcode.
      CALL METHOD c_oi_errors=>show_message
        EXPORTING
          type = 'E'.
    ENDIF.
  ENDIF.

***  find documents
  CHECK NOT proxy IS INITIAL.
*     straight ole automation
  DATA: document_cntl_handle TYPE cntl_handle.
  INCLUDE ole2incl.
  DATA: ocharacters  TYPE ole2_object,
        orange       TYPE ole2_object,
        oreplacement TYPE ole2_object,
        ofind        TYPE ole2_object,
        ofont        TYPE ole2_object.

  DATA: char_count    TYPE i,
        char_position TYPE i,
*            old_search_string LIKE search,
        string_found  TYPE i,
        color_index   TYPE i.

  CALL METHOD proxy->get_document_handle
    IMPORTING
      handle  = document_cntl_handle
      retcode = retcode.
  CALL METHOD c_oi_errors=>show_message
    EXPORTING
      type = 'E'.
*        get number of document characters.
  GET PROPERTY OF document_cntl_handle-obj
                 'characters' = ocharacters.
  GET PROPERTY OF ocharacters 'count' = char_count.
  char_position = 0.
*        old_search_string = search.

*     set range now
  IF char_position >= char_count.
    char_position = 0.
  ENDIF.

  GET PROPERTY OF document_cntl_handle-obj 'CONTENT' = gs_range.
  LOOP AT gt_value INTO gs_value.
    PERFORM replace_text USING gs_value-name gs_value-value.
  ENDLOOP.

  IF NOT bds_instance IS INITIAL.
    FREE bds_instance.
  ENDIF.
  IF NOT proxy IS INITIAL.
    FREE proxy.
  ENDIF.
  IF NOT control IS INITIAL.
    FREE control.
  ENDIF.

  IF NOT link_server_decl IS INITIAL.
    CALL METHOD link_server_decl->stop_link_server
      IMPORTING
        retcode = retcode.
    FREE link_server_decl.
  ENDIF.

*  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  replace_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIND_TEXT     text
*      -->P_REPLACE_TEXT  text
*----------------------------------------------------------------------*
FORM replace_text USING p_find_text p_replace_text.

  GET PROPERTY OF gs_range 'FIND' = gs_find.
  CALL METHOD OF
    gs_find
    'Execute'
    EXPORTING
      #1 = p_find_text.
  IF p_find_text NE '<<FOTO>>'.
    GET PROPERTY OF gs_find 'Found' = found.
    IF found > 0.

      CALL METHOD OF
        gs_find
        'EXECUTE'
        EXPORTING
          #1  = p_find_text
          #2  = 'False'
          #3  = 'False'
          #4  = 'False'
          #5  = 'False'
          #6  = 'False'
          #7  = 'True'
          #8  = '1'
          #9  = 'True'
          #10 = p_replace_text
          #11 = '2'
          #12 = 'True'
          #13 = 'True'
          #14 = 'True'
          #15 = 'True'.

    ENDIF.
  ELSE.


    DATA: lv_pernr TYPE pernr_d.

    CLEAR: lv_pernr.
    lv_pernr = p_replace_text.
    PERFORM download_photo USING lv_pernr.


    CONCATENATE 'C:\temp' '\' lv_pernr '.jpg'
     INTO gv_url.

    DATA: h_logo      TYPE ole2_object.
    DATA: inlineshapes      TYPE ole2_object.
    DATA: h_ins_logo      TYPE ole2_object.
    DATA: h_ins_logo_shape      TYPE ole2_object.
    DATA: h_logo_left      TYPE ole2_object.
    DATA: h_pos_logo      TYPE ole2_object.

    GET PROPERTY OF gs_range 'InlineShapes' = inlineshapes.
    CALL METHOD OF inlineshapes 'AddPicture' = h_ins_logo
           EXPORTING
           #1 = gv_url
           #2 = '10'
           #3 = '15'.

    CALL METHOD OF h_ins_logo_shape 'ConvertToShape' =  h_ins_logo.
    DATA: lw_height TYPE i,
          lw_width  TYPE i.
*    Get property of picture
    CLEAR : lw_height, lw_width.
    GET PROPERTY OF h_ins_logo 'Height' = lw_height.
    GET PROPERTY OF h_ins_logo 'Width'  = lw_width.
    CALL FUNCTION 'FLUSH'.

    PERFORM delete_photo USING lv_pernr.

    lw_height = 100.
    lw_width = 100.
*  Change the property
    SET PROPERTY OF h_ins_logo 'LockAspectRatio' = 0.
    SET PROPERTY OF h_ins_logo 'Height' = lw_height.
    SET PROPERTY OF h_ins_logo 'Width' = lw_width.
*      SET PROPERTY OF h_ins_logo 'Left' = 155.
*      SET PROPERTY OF h_ins_logo 'Top' =  30.
    GET PROPERTY OF gs_find 'Found' = found.
    IF found > 0.

      CLEAR: p_replace_text.
      CALL METHOD OF
        gs_find
        'EXECUTE'
        EXPORTING
          #1  = p_find_text
          #2  = 'False'
          #3  = 'False'
          #4  = 'False'
          #5  = 'False'
          #6  = 'False'
          #7  = 'True'
          #8  = '1'
          #9  = 'True'
          #10 = p_replace_text
          #11 = '2'
          #12 = 'True'
          #13 = 'True'
          #14 = 'True'
          #15 = 'True'.
* insert the picture in default (top left) POSITION
    ENDIF.
*    "--<<add code  by mkarabulut ~ end

  ENDIF.

*      GET PROPERTY OF gs_find 'Found' = found.
*    IF found > 0.
*
*      CALL METHOD OF
*        gs_find
*        'EXECUTE'
*        EXPORTING
*          #1  = p_find_text
*          #2  = 'False'
*          #3  = 'False'
*          #4  = 'False'
*          #5  = 'False'
*          #6  = 'False'
*          #7  = 'True'
*          #8  = '1'
*          #9  = 'True'
*          #10 = p_replace_text
*          #11 = '2'
*          #12 = 'True'
*          #13 = 'True'
*          #14 = 'True'
*          #15 = 'True'.
*
*    ENDIF.


ENDFORM.                    "replace_text
FORM download_photo USING pernr TYPE pernr_d.

  DATA: url(255)       TYPE c,
        p_connect_info LIKE TABLE OF toav0 WITH HEADER LINE,
        handle         TYPE i.

  DATA :  l_current TYPE xstring.
  DATA : ex_document TYPE TABLE OF  tbl1024,
         ex_length   TYPE int4.
  DATA : binary_tab TYPE TABLE OF tbl1024,
         buffer     TYPE xstring.
  DATA : v_filename TYPE string.
  TYPES : BEGIN OF ty_binary,
            binary_field(1000) TYPE c,
          END OF ty_binary.

  DATA : li_data TYPE TABLE OF ty_binary WITH HEADER LINE.

  CALL FUNCTION 'HR_IMAGE_EXISTS'
    EXPORTING
      p_pernr        = pernr
    IMPORTING
      p_connect_info = p_connect_info
    EXCEPTIONS
      OTHERS         = 2.

  IF sy-subrc EQ 0.

    CALL FUNCTION 'ALINK_RFC_TABLE_GET'
      EXPORTING
        im_docid    = p_connect_info-arc_doc_id
        im_crepid   = p_connect_info-archiv_id
*       IM_COMPID   =
      IMPORTING
        ex_length   = ex_length
*       EX_MESSAGE  =
      TABLES
        ex_document = ex_document.
    binary_tab[] = ex_document[].

    IF ex_length IS INITIAL.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = ex_length
*       FIRST_LINE   = 0
*       LAST_LINE    = 0
      IMPORTING
        buffer       = buffer
      TABLES
        binary_tab   = binary_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = buffer
*       APPEND_TO_TABLE       = # #
* IMPORTING
*       OUTPUT_LENGTH         =
      TABLES
        binary_tab = li_data.
    CONCATENATE 'C:\temp' '\' pernr '.jpg'
     INTO v_filename.
    CONDENSE v_filename.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
*       BIN_FILESIZE                    =
        filename = v_filename
        filetype = 'BIN'
* IMPORTING
*       FILELENGTH                      =
      TABLES
        data_tab = li_data.
    IF sy-subrc EQ 0.
      gv_flag = 1.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETE_PHOTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_PERNR_PERNR  text
*----------------------------------------------------------------------*
FORM delete_photo  USING pernr TYPE pernr_d.

  IF gv_flag EQ 1.

    DATA v_rc TYPE i.
    DATA : v_filename TYPE string.
    CONCATENATE 'C:\temp' '\' pernr '.jpg'
       INTO v_filename.
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename             = v_filename
      CHANGING
        rc                   = v_rc
      EXCEPTIONS
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.
ENDFORM.                    " DELETE_PHOTO


CLASS create_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main IMPORTING
                         VALUE(typ)  TYPE c
                         VALUE(len)  TYPE i
                         VALUE(dec)  TYPE i
                       RETURNING
                         VALUE(dref) TYPE REF TO data.
  PRIVATE SECTION.
    CLASS-METHODS create_data IMPORTING
                                        VALUE(typ)  TYPE c
                                        VALUE(len)  TYPE i
                                        VALUE(dec)  TYPE i
                              RETURNING
                                        VALUE(dref) TYPE REF TO data
                              RAISING   cx_sy_create_data_error.
ENDCLASS.

CLASS create_demo IMPLEMENTATION.
  METHOD main.
    FIELD-SYMBOLS <fs> TYPE any.

    TRY.
        dref = create_data( typ = typ
                            len = len
                            dec = dec ).
        ASSIGN dref->* TO <fs>.
        DESCRIBE FIELD <fs> TYPE typ
                            LENGTH len IN BYTE MODE
                            DECIMALS dec.
      CATCH cx_sy_create_data_error.
    ENDTRY.
  ENDMETHOD.
  METHOD create_data.
    TRANSLATE typ TO LOWER CASE.
    CASE typ.
      WHEN 'd' OR 'decfloat16' OR 'decfloat34' OR 'f' OR 'i'
               OR 'string' OR 't' OR 'xstring'.
        CREATE DATA dref TYPE (typ).
      WHEN 'c' OR 'n' OR 'x'.
        CREATE DATA dref TYPE (typ) LENGTH len.
      WHEN 'p'.
        CREATE DATA dref TYPE p LENGTH len DECIMALS dec.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_sy_create_data_error.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

DATA : gc_data TYPE REF TO create_demo.
