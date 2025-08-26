*&---------------------------------------------------------------------*
*& Include          zhrbr_P001_I003
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    METHODS :
      set_init,
      at_selection_screen ,
*      screen_output,
      send_word,
      get_data.
*      set_data ,
*      set_texts,
*      prepare_alv,
*      display_alv,
*      salv_variant
*        CHANGING cv_layout TYPE disvariant-variant.
*
*  PROTECTED SECTION.
*    DATA : gr_alv       TYPE REF TO cl_salv_table,
*           gr_past      TYPE REF TO cl_salv_table,
*           gr_display   TYPE REF TO cl_salv_display_settings,
*           gr_columns   TYPE REF TO cl_salv_columns_table,
*           gr_column    TYPE REF TO cl_salv_column_table,
*           gr_functions TYPE REF TO cl_salv_functions_list,
*           gr_selection TYPE REF TO  cl_salv_selections,
*           gr_layout    TYPE REF TO cl_salv_layout,
*           gr_events    TYPE REF TO cl_salv_events_table,
*           gr_exp_msg   TYPE REF TO cx_salv_msg.
*
*    DATA : gs_key     TYPE salv_s_layout_key,
*           gs_variant TYPE slis_vari.
*
*  PRIVATE    SECTION.
*    METHODS :
*      create_alv,
*      set_pf_status,
*      set_top_of_page,
*      set_alv_properties,
*      set_column_styles,
*      set_column_text
*        IMPORTING i_fname TYPE lvc_fname
*                  i_text  TYPE any.
*
*    METHODS : on_user_command
*                  FOR EVENT added_function OF cl_salv_events
*      IMPORTING e_salv_function.


ENDCLASS.                    "lcl_report DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.
  METHOD set_init.

  ENDMETHOD.                    "set_init
*
  METHOD at_selection_screen.

    TYPE-POOLS : vrm .
    DATA : t_value TYPE  vrm_values .


    SELECT dosno FROM pa9950 INTO TABLE t_value
                       WHERE pernr EQ p_pernr .

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'P_DOSNO'
        values          = t_value
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.


  ENDMETHOD.                    "at_selection_screen
*
*  METHOD screen_output.
*  ENDMETHOD.                    "screen_output
*
  METHOD send_word.
    DATA : BEGIN OF ls_adrc,
             name1 TYPE adrc-name1,
             name2 TYPE adrc-name2,
           END OF   ls_adrc.

    CLEAR :  gt_9950, gs_9950, gt_word, gs_word.
    " personel numarası, ismi, şirket bilgisi ve TC no okunur.
    gs_report-pernr = p_pernr.
    SELECT SINGLE ename bukrs FROM pa0001 INTO (gs_report-ename, gs_report-bukrs)
                              WHERE pernr EQ p_pernr
                                AND begda LE sy-datum
                                AND endda EQ '99991231'.
    SELECT SINGLE merni FROM pa0770 INTO gs_report-merni
                              WHERE pernr EQ p_pernr
                                AND begda LE sy-datum
                                AND endda EQ '99991231'.
**** Comment EG EGULDAL 14.04.2016
*    SELECT SINGLE butxt FROM t001 INTO gs_report-butxt
*                              WHERE bukrs EQ gs_report-bukrs.
**** Comment EG EGULDAL 14.04.2016

**** Insert EG EGULDAL 14.04.2016
    DATA : lv_adrnr TYPE t001-adrnr .
    SELECT SINGLE adrnr FROM t001 INTO lv_adrnr WHERE bukrs = gs_report-bukrs.
    IF lv_adrnr IS NOT INITIAL.
      SELECT SINGLE name1 name2 FROM adrc INTO ls_adrc
        WHERE addrnumber = lv_adrnr.

      IF sy-subrc = 0.
        CONCATENATE ls_adrc-name1 ls_adrc-name2 INTO gs_report-butxt SEPARATED BY space.
      ENDIF.
    ENDIF.
**** Insert EG EGULDAL 14.04.2016


    CASE number.
      WHEN 110.  PERFORM set_110.
      WHEN 120.  PERFORM set_120.
      WHEN 130.  PERFORM set_130.
      WHEN 140.  PERFORM set_140.
    ENDCASE.

  ENDMETHOD.                    "send_word
*
*  METHOD set_data.
*  ENDMETHOD.                    "set_data
  METHOD get_data .
    DATA : s9950 TYPE pa9950,
           s0006 TYPE pa0006.

    DATA : lv_bezei TYPE t005u-bezei,
           lv_icrad TYPE zhrbr_t006-icraa. " İcra Dairesi Adı

    SELECT SINGLE * FROM pa9950 INTO s9950
                   WHERE pernr EQ p_pernr
                     AND dosno EQ p_dosno .

    gs_report-dosno  = s9950-dosno.

    SELECT SINGLE icraa FROM zhrbr_t006 INTO lv_icrad
                           WHERE icrad = s9950-icrad .

    SELECT SINGLE * FROM pa0006 INTO s0006
                        WHERE pernr EQ p_pernr
                          AND begda LE sy-datum
                          AND endda GE sy-datum .

    CONCATENATE  s0006-stras  s0006-locat INTO gs_report-adr "p1_icryr
              SEPARATED BY space .

    SELECT SINGLE bezei FROM t005u INTO lv_bezei
                       WHERE spras EQ sy-langu
                         AND land1 EQ 'TR'
                         AND bland EQ s0006-state .

    CONCATENATE  s0006-ort02 '/' lv_bezei INTO gs_report-adr2  "p1_icry2
            SEPARATED BY space .



    SELECT SINGLE usrid FROM  pa0105 INTO gs_report-tel     "p1_usrid
                       WHERE pernr EQ p_pernr
                         AND subty EQ 'CELL' .

    p1_dosno = gs_report-dosno .
*    p1_icryr = gs_report-adr .
*    p1_icry2 = gs_report-adr2 .
*    p1_usrid = gs_report-tel .
    p1_icrad = lv_icrad .                        " NERDEN AINACAK TARİH?
    p1_evtrh = p2_evtrh = p3_evtrh = p4_evtrh = sy-datum ."s9950-zhr_ic_kestrh.

    p2_kytno = gs_report-dosno .
    p3_kytno = gs_report-dosno .

    p4_dosno = gs_report-dosno .
*    p4_icryr = gs_report-adr .
*    p4_icry2 = gs_report-adr2 .
*    p4_usrid = gs_report-tel .
    p4_icrad = lv_icrad .


  ENDMETHOD .                    "get_data
*
*  METHOD set_texts.
**    CALL FUNCTION 'ZUSGN_FG001_001'
**      CHANGING
**        textt = gt_txt.
**
**    LOOP AT gt_txt INTO gs_txt.
**      CASE gs_txt-flag.
**        WHEN 'BUKRS'.
**          gs_report-bukrs_t = gs_txt-txt01.
**          MODIFY gt_report FROM gs_report TRANSPORTING bukrs_t
**                                          WHERE bukrs EQ gs_txt-val01.
**      ENDCASE.
**      CLEAR gs_report.
**    ENDLOOP.
*
*  ENDMETHOD.                    "set_texts
*
*  METHOD prepare_alv.
*    me->create_alv( ).
*    me->set_pf_status( ).
*    me->set_alv_properties( ).
*    me->set_top_of_page( ).
*    me->display_alv( ).
*  ENDMETHOD.                    "prepare_alv
*
*  METHOD create_alv.
*    TRY.
*        cl_salv_table=>factory(
*                  IMPORTING
*                    r_salv_table = gr_alv
*                  CHANGING
*                    t_table      = gt_report ).
*      CATCH
*        cx_salv_msg INTO gr_exp_msg.
*    ENDTRY.
*
*  ENDMETHOD.                    "create_alv
*
*  METHOD set_pf_status.
*    gr_alv->set_screen_status(
*                pfstatus      = 'GUI'
*                report        = sy-repid
*                set_functions = gr_alv->c_functions_all ).
*  ENDMETHOD.                    "set_pf_status
*
*  METHOD set_top_of_page.
** Uğur Tangül Rapor ismi - tarih - kullanıcı + logo
*    DATA : lo_header      TYPE REF TO cl_salv_form_layout_grid,
*           lo_grid_bottom TYPE REF TO cl_salv_form_layout_grid,
*           lo_logo        TYPE REF TO cl_salv_form_layout_logo,
*           lo_text        TYPE REF TO cl_salv_form_text,
*           lo_label       TYPE REF TO cl_salv_form_label.
*
*    DATA : lv_date  TYPE text10.
*
*    CREATE OBJECT lo_header.
*
*    lo_header->create_header_information(
*                    row    = 1
*                    column = 1
*                    text   = text-t01 )  . " Rapor ismi
*    lo_header->add_row( ).
*
*    lo_grid_bottom = lo_header->create_grid(
*                   row    = 3
*                   column = 1 ).
*
*    lo_label = lo_grid_bottom->create_label(
*                   row     = 1
*                   column  = 1
*                   text    = text-t02
*                   tooltip = text-t02 ). " Tarih
*
*    WRITE sy-datum TO lv_date DD/MM/YYYY.
*    lo_text = lo_grid_bottom->create_text(
*                   row     = 1
*                   column  = 2
*                   text    = lv_date
*                   tooltip = lv_date ). " Tarih
*
*    lo_label = lo_grid_bottom->create_label(
*                   row     = 2
*                   column  = 1
*                   text    = text-t03
*                   tooltip = text-t03 ). " Kullanıcı
*
*    lo_text = lo_grid_bottom->create_text(
*                   row     = 2
*                   column  = 2
*                   text    = sy-uname
*                   tooltip = sy-uname ). " Kullanıcı
*
*
*    lo_label->set_label_for( lo_text ).
*
*    CREATE OBJECT lo_logo.
*    lo_logo->set_left_content( lo_header ).
*    lo_logo->set_right_logo( 'UGUR_LOGO' ).
*
*    gr_alv->set_top_of_list( lo_logo ).
*
*  ENDMETHOD.                    "set_top_of_page
*
*  METHOD set_alv_properties.
*
*    gr_display = gr_alv->get_display_settings( ).
*
** Zebra sytle..
*    gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*
*    gr_columns = gr_alv->get_columns( ).
** Set optimize..
*    gr_columns->set_optimize( abap_true ).
*
*    gr_layout = gr_alv->get_layout( ).
** Set variant..
*    gs_key-report = sy-repid.
*    gr_layout->set_key( gs_key ).
*    gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
*
** Alv default settings..
*    gr_layout->set_default( if_salv_c_bool_sap=>true ).
*
** Set selection..
*    gr_selection = gr_alv->get_selections( ).
*    gr_selection->set_selection_mode( if_salv_c_selection_mode=>cell ).
*
*    gr_events = gr_alv->get_event( ).
** Set ALV Events.
*    SET HANDLER gr_report->on_user_command FOR gr_events.
*
** Set Column Texts.
**    me->set_column_text( i_fname = 'BEGDA'   i_text = text-003 ).
*
**    me->set_column_styles( ).
*
** Hide columns.
**    gr_column ?= gr_columns->get_column( 'SUBTY' ).
**    gr_column->set_visible( if_salv_c_bool_sap=>false ).
*
*  ENDMETHOD.                    "set_alv_properties
*
*  METHOD set_column_styles.
*
**    data : ls_color type lvc_s_colo.
**
**    data : lv_index(1) type c,
**           lv_column   type lvc_fname.
**
**    move : '7' to ls_color-col,
**           '0' to ls_color-int,
**           '0' to ls_color-inv.
**
**    do 4 times.
**      move : sy-index to lv_index.
**      concatenate 'STAT' lv_index into lv_column.
**      gr_column ?= gr_columns->get_column( lv_column ).
**      gr_column->set_color( ls_color ).
**      gr_column->set_alignment( if_salv_c_alignment=>centered ).
**      gr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
**    enddo.
*
*  ENDMETHOD.                    "set_column_styles
*
*  METHOD set_column_text.
*
*    DATA : lv_ltext TYPE scrtext_l,
*           lv_mtext TYPE scrtext_m,
*           lv_stext TYPE scrtext_s.
*
*    gr_column ?= gr_columns->get_column( i_fname ).
*    MOVE : i_text TO lv_ltext.
*    gr_column->set_long_text( lv_ltext ).
*    MOVE : i_text TO lv_mtext.
*    gr_column->set_medium_text( lv_mtext ).
*    MOVE : i_text TO lv_stext.
*    gr_column->set_short_text( lv_stext  ).
*
*  ENDMETHOD.                    "set_column_text
*
*  METHOD display_alv.
*    gr_alv->display( ).
*  ENDMETHOD.                    "display_alv
*
*  METHOD salv_variant.
*
*    DATA: ls_layout TYPE salv_s_layout_info,
*          ls_key    TYPE salv_s_layout_key.
*
*    ls_key-report = sy-repid.
*
*    ls_layout = cl_salv_layout_service=>f4_layouts(
*                  s_key    = ls_key
*                  restrict = if_salv_c_layout=>restrict_none  ).
*
*    cv_layout = ls_layout-layout.
*
*
*  ENDMETHOD.                    "for_salv
*
*  METHOD on_user_command.
**
**    DATA : lt_rows  TYPE salv_t_row,
**           lv_rows  TYPE int4,
**           lv_subrc TYPE sy-subrc.
**
**    CASE e_salv_function.
**      WHEN 'STAT1' OR 'STAT2' OR 'STAT3' OR 'STAT4' OR 'CANCEL'.
**
**        lt_rows = gr_selection->get_selected_rows( ).
**      WHEN OTHERS.
**    ENDCASE.
*
*    gr_alv->refresh( ).
*
*  ENDMETHOD.                    "on_user_command

ENDCLASS. "lcl_report IMPLEMENTATION
