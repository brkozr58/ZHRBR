*&---------------------------------------------------------------------*
*& Include          ZETPA_P029_I003
*&---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
    METHODS :
      set_init,
      set_date,
      hr_get_subtype_text
        IMPORTING i_subty TYPE subty
        CHANGING  c_stext TYPE sbttx,
      get_person,
      prepare_alv,
      pdf IMPORTING rows TYPE salv_t_row,
      display_alv.

  PROTECTED SECTION.
    DATA : gr_alv       TYPE REF TO cl_salv_table,
           gr_past      TYPE REF TO cl_salv_table,
           gr_display   TYPE REF TO cl_salv_display_settings,
           gr_columns   TYPE REF TO cl_salv_columns_table,
           gr_column    TYPE REF TO cl_salv_column_table,
           gr_functions TYPE REF TO cl_salv_functions_list,
           gr_selection TYPE REF TO cl_salv_selections,
           gr_layout    TYPE REF TO cl_salv_layout,
           gr_events    TYPE REF TO cl_salv_events_table,
           gr_exp_msg   TYPE REF TO cx_salv_msg.

    DATA : gs_key   TYPE salv_s_layout_key.

  PRIVATE SECTION.
    METHODS :
      create_alv,
      set_pf_status,
      set_top_of_page,
      set_alv_properties,
      set_column_styles,
      set_column_text
        IMPORTING i_fname TYPE lvc_fname
                  i_text  TYPE any,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

ENDCLASS.                    "lcl_report DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.
  METHOD set_init.
    DATA : rs_datum LIKE LINE OF s_datum.
    DATA : lv_datum TYPE sy-datum.
    SELECT * FROM zhrbr_t006 INTO TABLE gt_t006.
    IF s_datum[] IS INITIAL.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = sy-datum
        IMPORTING
          last_day_of_month = lv_datum.

      rs_datum = 'IEQ'.
      rs_datum-low  = sy-datum(6) && '01'.
      rs_datum-high = lv_datum.
      APPEND rs_datum TO s_datum.
    ENDIF.

    SELECT * FROM t001p INTO CORRESPONDING FIELDS OF TABLE t001p[] .
    SELECT * FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE
      hrp1000[] WHERE istat = '1' AND langu = 'TR'  AND
      endda = '99991231' .
    SELECT * FROM t549t INTO CORRESPONDING FIELDS OF TABLE t549t[]
      WHERE sprsl = 'TR' .

    PERFORM init.
  ENDMETHOD.                    "set_init
  METHOD set_date.

    IF s_datum-high IS INITIAL.
      s_datum-high = s_datum-low.
    ENDIF.

    pn-begps = pnpbegps = pn-begda = pnpbegda = s_datum-low  .
    pn-endps = pnpendps = pn-endda = pnpendda = s_datum-high .

    SELECT * FROM t591s INTO TABLE t591s WHERE sprsl EQ sy-langu
                                           AND infty EQ '9950' .
  ENDMETHOD.                    "set_date
  METHOD prepare_alv.
    me->create_alv( ).
    me->set_pf_status( ).
    me->set_alv_properties( ).
    me->set_top_of_page( ).
    me->display_alv( ).
  ENDMETHOD.                    "prepare_alv

  METHOD create_alv.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = gr_alv
          CHANGING
            t_table      = gt_report ).
      CATCH
        cx_salv_msg INTO gr_exp_msg.
    ENDTRY.

  ENDMETHOD.                    "create_alv

  METHOD set_pf_status.
    gr_alv->set_screen_status(
      pfstatus      = 'GUI'
      report        = sy-repid
      set_functions = gr_alv->c_functions_all ).
  ENDMETHOD.                    "set_pf_status

  METHOD set_top_of_page.

    DATA : lo_header      TYPE REF TO cl_salv_form_layout_grid,
           lo_grid_bottom TYPE REF TO cl_salv_form_layout_grid,
           lo_logo        TYPE REF TO cl_salv_form_layout_logo,
           lo_text        TYPE REF TO cl_salv_form_text,
           lo_label       TYPE REF TO cl_salv_form_label.

    DATA : lv_date  TYPE text10.

    CREATE OBJECT lo_header.

    lo_header->create_header_information(
      row    = 1
      column = 1
      text   = text-t01 ). " Rapor ismi
    lo_header->add_row( ).

    lo_grid_bottom = lo_header->create_grid(
      row    = 3
      column = 1 ).

    lo_label = lo_grid_bottom->create_label(
      row     = 1
      column  = 1
      text    = text-t02
      tooltip = text-t02 ). " Tarih

    WRITE sy-datum TO lv_date DD/MM/YYYY.
    lo_text = lo_grid_bottom->create_text(
      row     = 1
      column  = 2
      text    = lv_date
      tooltip = lv_date ). " Tarih

    lo_label = lo_grid_bottom->create_label(
      row     = 2
      column  = 1
      text    = text-t03
      tooltip = text-t03 ). " Kullanıcı

    lo_text = lo_grid_bottom->create_text(
      row     = 2
      column  = 2
      text    = sy-uname
      tooltip = sy-uname ). " Kullanıcı


    lo_label->set_label_for( lo_text ).


  ENDMETHOD.                    "set_top_of_page

  METHOD set_alv_properties.

    gr_display = gr_alv->get_display_settings( ).

* Zebra sytle..
    gr_display->set_striped_pattern( cl_salv_display_settings=>true ).

    gr_columns = gr_alv->get_columns( ).
* Set optimize..
    gr_columns->set_optimize( abap_true ).

    gr_layout = gr_alv->get_layout( ).
* Set variant..
    gs_key-report = sy-repid.
    gr_layout->set_key( gs_key ).
    gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

* Set selection..
    gr_selection = gr_alv->get_selections( ).
    gr_selection->set_selection_mode( if_salv_c_selection_mode=>cell ).

    gr_events = gr_alv->get_event( ).
* Set ALV Events.
    SET HANDLER gr_report->on_user_command FOR gr_events.

* Set Column Texts.
    me->set_column_text( i_fname = 'PERNR' i_text = text-001 ).
    me->set_column_text( i_fname = 'ENAME' i_text = text-002 ).
    me->set_column_text( i_fname = 'DOSNO' i_text = text-003 ).
    me->set_column_text( i_fname = 'ICRAA' i_text = text-004 ).
    me->set_column_text( i_fname = 'ALICI' i_text = text-005 ).
    me->set_column_text( i_fname = 'AIBAN' i_text = text-006 ).
    me->set_column_text( i_fname = 'ABORC' i_text = text-007 ).
    me->set_column_text( i_fname = 'DONEM' i_text = text-008 ).
    me->set_column_text( i_fname = 'ODMTR' i_text = text-009 ).
    me->set_column_text( i_fname = 'TKEST' i_text = text-010 ).
    me->set_column_text( i_fname = 'BBORC' i_text = text-011 ).
    me->set_column_text( i_fname = 'SUBTX' i_text = text-012 ).
*    me->set_column_text( i_fname = 'SUBTY'  i_text = TEXT-013 ).



    me->set_column_styles( ).
*    HIDE columns.
    gr_column ?= gr_columns->get_column( 'ICRAD' ).
    gr_column->set_technical(  if_salv_c_bool_sap=>true ).
*    HIDE columns.
*    gr_column ?= gr_columns->get_column( 'SUBTX' ).
*    gr_column->set_technical(  if_salv_c_bool_sap=>true ).


  ENDMETHOD.                    "set_alv_properties

  METHOD set_column_styles.
  ENDMETHOD.                    "set_column_styles

  METHOD set_column_text.

    DATA : lv_ltext TYPE scrtext_l,
           lv_mtext TYPE scrtext_m,
           lv_stext TYPE scrtext_s.

    gr_column ?= gr_columns->get_column( i_fname ).
    MOVE : i_text TO lv_ltext.
    gr_column->set_long_text( lv_ltext ).
    MOVE : i_text TO lv_mtext.
    gr_column->set_medium_text( lv_mtext ).
    MOVE : i_text TO lv_stext.
    gr_column->set_short_text( lv_stext  ).

  ENDMETHOD.                    "set_column_text
  METHOD display_alv.
    gr_alv->display( ).
  ENDMETHOD.                    "display_alv
  METHOD get_person.
    DATA : ls_report TYPE zhrbr_s003,
           lv_borc   TYPE betrg,
           lv_date   TYPE datum.


    DATA : lv_years  TYPE  pea_scryy,
           lv_months TYPE  pea_scrmm,
           lv_days   TYPE  pea_scrdd,
           lv_m      TYPE i.

    CLEAR: ls_report.

    CHECK pernr-pernr IN pnppernr AND
          pernr-bukrs IN pnpbukrs AND
          pernr-werks IN pnpwerks AND
          pernr-btrtl IN pnpbtrtl AND
          pernr-persg IN pnppersg AND
          pernr-persk IN pnppersk AND
          pernr-kostl IN pnpkostl AND
          pernr-plans IN pnpplans AND
          pernr-orgeh IN pnporgeh AND
          pernr-stell IN pnpstell AND
          pernr-abkrs IN pnpabkrs AND
          pernr-ansvh IN pnpansvh AND
          pernr-vdsk1 IN pnpvdsk1.

    DATA : ls_p9950 TYPE p9950.
    DATA : s9951    TYPE p9951.
    DATA : lv_betrg TYPE betrg .



    IF p9951[] IS NOT INITIAL AND p9950[] IS INITIAL.
      SELECT * FROM pa9950 INTO CORRESPONDING FIELDS OF TABLE p9950
            WHERE pernr EQ pernr-pernr
              AND begda LE pnpendda
              AND icrid EQ p9951-icrid
              AND subty EQ '3560' .
    ENDIF .


    DATA : t026 TYPE TABLE OF zhrbr_t008.

    SELECT * FROM zhrbr_t008 INTO TABLE t026 WHERE pernr EQ pernr-pernr.

    LOOP AT p9950 INTO ls_p9950 WHERE subty IN s_subty.

      LOOP AT p9951 INTO s9951 WHERE begda LE s_datum-high
                                       AND endda GE s_datum-low
                                       AND icrid EQ ls_p9950-icrid.
      ENDLOOP.

*      CHECK sy-subrc EQ 0.
      lv_date = s_datum-high + 1.

      IF p_fin EQ 'X'.
        CLEAR lv_betrg .
        CALL METHOD zhrbr_cl001=>odenen_borc
          EXPORTING
            icrid = ls_p9950-icrid
            begda = lv_date
          IMPORTING
            icodm = lv_betrg. "ls_report-odmtr.

        ls_report-odmtr = lv_betrg .

        ls_report-aborc = ls_p9950-icrtr + ls_p9950-faizt .
        CLEAR: lv_borc.
        lv_borc = ls_report-aborc - ls_report-odmtr.
        CHECK lv_borc EQ 0.

      ENDIF.
      "organizason text .
      ls_report-orgeh = p0001-orgeh .
      CLEAR hrp1000 .
      READ TABLE hrp1000[] INTO hrp1000 WITH KEY   plvar = '01'
                                                   otype = 'O'
                                                   objid = p0001-orgeh
                                                   langu = 'TR'
                                                   endda = '99991231'
                                                   istat = '1'.
      ls_report-ortext = hrp1000-stext .

      "pozisyon text
      CLEAR hrp1000.
      READ TABLE hrp1000[] INTO hrp1000 WITH KEY  plvar = '01'
                                                  otype = 'S'
                                                  objid =  p0001-plans
                                                  langu = 'TR'
                                                  endda = '99991231'
                                                  istat = '1'.
      ls_report-pstext = hrp1000-stext .
      ls_report-plans = p0001-plans .

      "stell
      CLEAR hrp1000.
      READ TABLE hrp1000[] INTO hrp1000 WITH KEY  plvar = '01'
                                                  otype = 'C'
                                                  objid =  p0001-stell
                                                  langu = 'TR'
                                                  endda = '99991231'
                                                  istat = '1' .
      ls_report-stell  = p0001-stell .
      ls_report-sstext = hrp1000-stext .

      ls_report-abkrs = p0001-abkrs.
      CLEAR t549t .
      READ TABLE t549t[] INTO t549t WITH KEY abkrs = p0001-abkrs .
      ls_report-abktx = t549t-atext.

      "tc kimlik no
      ls_report-merni = p0770-merni .

      "personel alt alanı
      CLEAR t001p .
      READ TABLE t001p[] INTO t001p WITH KEY
      werks = p0001-werks btrtl = p0001-btrtl .

      ls_report-btext = t001p-btext.
      ls_report-btrtl = p0001-btrtl  .
      ls_report-stell = p0001-stell .

      ls_report-pernr = p0001-pernr.
      ls_report-ename = p0001-ename.
      ls_report-icrad = ls_p9950-icrad.
      ls_report-dosno = ls_p9950-dosno.
      ls_report-alici = ls_p9950-alici.
      ls_report-aiban = ls_p9950-aiban.
      ls_report-kessr = ls_p9950-kessr.
      ls_report-aborc = ls_p9950-icrtr + ls_p9950-faizt .


      SORT p0000 BY endda DESCENDING .

      READ TABLE p0000[] INTO p0000 WITH KEY massn = '12'.
      IF sy-subrc EQ  0 .
        ls_report-isgir = p0000-begda .
        ls_report-iscik = ''.
      ELSE.
        READ TABLE p0000[] INTO p0000 WITH KEY massn = '01' .
        ls_report-isgir = p0000-begda .

        READ TABLE p0000[] INTO p0000 WITH KEY massn = '10' .
        IF sy-subrc EQ 0 .
          ls_report-iscik = p0000-begda - 1 .
        ENDIF.

      ENDIF.


*      ls_report-subty = ls_p9950-subty .
      DATA : s_t591s TYPE t591s .
      READ TABLE t591s INTO s_t591s WITH KEY subty = ls_p9950-subty .

      ls_report-subtx = s_t591s-stext .

      CLEAR s_t591s .

      IF ls_report-aborc IS INITIAL AND ls_p9950-hspsk EQ '01'.
        ls_report-aborc = ls_p9950-tkstt.
      ENDIF.

      IF ls_p9950-subty EQ '3560' AND ls_p9950-hspsk  EQ '01' .

        CLEAR : lv_years ,lv_months ,lv_days ,lv_m .
        CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
          EXPORTING
            date1                       = ls_p9950-endda
            date2                       = ls_p9950-begda
            output_format               = '05'
          IMPORTING
            years                       = lv_years
            months                      = lv_months
            days                        = lv_days
          EXCEPTIONS
            overflow_long_years_between = 1
            invalid_dates_specified     = 2
            OTHERS                      = 3.

        lv_m = trunc( lv_months ).
        lv_m = lv_m + ( lv_years * 12 ).
        ls_report-aborc = lv_m * ls_p9950-tkstt .

      ENDIF .
      DATA : s026 LIKE LINE OF t026.
      READ TABLE t026 INTO s026 WITH KEY pernr = pernr-pernr
                                               icrid = ls_p9950-icrid.
      IF sy-subrc EQ 0.
        ls_report-aborc = ls_report-aborc + s026-odmtr.
      ENDIF.
*
      READ TABLE gt_t006 INTO ls_t006
                   WITH  KEY icrad = ls_p9950-icrad.
      IF sy-subrc EQ 0.
        ls_report-icraa = ls_t006-icraa.
        CLEAR: ls_t006.

      ENDIF.
      LOOP AT p9951 INTO s9951 WHERE icrid EQ ls_p9950-icrid
                                 AND begda LE s_datum-high
                                 AND endda GE s_datum-low.
        ls_report-odmtr = s9951-odmtr.
        ls_report-donem = s9951-begda(6).
        lv_date = s9951-begda + 1."Sonraki ayın ilk gününden küçükse
        CALL METHOD zhrbr_cl001=>odenen_borc
          EXPORTING
            icrid = ls_p9950-icrid
            begda = lv_date
          IMPORTING
            icodm = ls_report-tkest.

        IF ls_report-tkest GT ls_report-aborc .
          ls_report-aborc =  ls_report-tkest .
        ENDIF .

        ls_report-bborc = ls_report-aborc - ls_report-tkest.

        ls_report-compl = ls_p9950-compl ."e
        IF ls_report-compl EQ 'D' ..
          ls_report-compltx = 'Ara Ver' .
        ELSEIF ls_report-compl = '' .
          ls_report-compltx = 'Açık' .
        ENDIF. "e

        APPEND ls_report TO gt_report.
      ENDLOOP.
      IF sy-subrc NE 0.
        IF ls_p9950-subty EQ '3560' AND ls_p9950-hspsk  EQ '01' .

          LOOP AT p9951 INTO s9951 WHERE icrid EQ ls_p9950-icrid
                            AND endda LE s_datum-low .

            ls_report-tkest = ls_report-tkest + s9951-odmtr .
          ENDLOOP .

          IF ls_report-tkest GT ls_report-aborc .
            ls_report-aborc =  ls_report-tkest .
          ENDIF .

          ls_report-bborc = ls_report-aborc - ls_report-tkest.

        ELSEIF ls_p9950-subty EQ '3560' AND ls_p9950-hspsk  EQ '02' .

          LOOP AT p9951 INTO s9951 WHERE icrid EQ ls_p9950-icrid
                                  AND begda LE s_datum-high .ENDLOOP .
          ls_report-odmtr = s9951-odmtr.
          ls_report-donem = s9951-begda(6).
          lv_date = s9951-begda + 1."Sonraki ayın ilk gününden küçükse
          CALL METHOD zhrbr_cl001=>odenen_borc
            EXPORTING
              icrid = ls_p9950-icrid
              begda = lv_date
            IMPORTING
              icodm = ls_report-tkest.
          ls_report-bborc = ls_report-aborc - ls_report-tkest.

          IF ls_report-donem IS INITIAL AND p9950-begda IN s_datum .
            ls_report-donem = ls_p9950-begda(6).
          ENDIF .

        ELSE .

          ls_report-bborc = ls_report-aborc .

        ENDIF .

        IF ls_report-bborc EQ 0 AND ls_report-donem LT s_datum-low(6) .
          " geçmiş aylarda bitenler gelmesin
        ELSE .
          ls_report-compl = ls_p9950-compl ."e
          IF ls_report-compl EQ 'D' ..
            ls_report-compltx = 'Ara Ver' .
          ELSEIF ls_report-compl = '' .
            ls_report-compltx = 'Açık' .

          ENDIF.
          APPEND ls_report TO gt_report."e

        ENDIF .
      ENDIF.
      CLEAR ls_report.
    ENDLOOP.



    DELETE gt_report WHERE  donem IS  INITIAL .
    DELETE gt_report WHERE  donem LT s_datum-low(6) .

  ENDMETHOD.                    "get_person
  METHOD hr_get_subtype_text.

    CALL FUNCTION 'HR_GET_SUBTYPE_TEXT'
      EXPORTING
        infty               = '9950'
        subty               = i_subty
      IMPORTING
        stext               = c_stext
      EXCEPTIONS
        infty_not_found     = 1
        subty_not_found     = 2
        infty_not_supported = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.                    "hr_get_subtype_text
  METHOD on_user_command.
    DATA : lt_rows  TYPE salv_t_row,
           lv_rows  TYPE int4,
           lv_subrc TYPE sy-subrc.
*
    CASE e_salv_function.
      WHEN '&F03' OR '&F15' OR '&F12'.
        CALL  SCREEN 0.
*      WHEN '&PDF'.
*        lt_rows = gr_selection->get_selected_rows( ).
*        DESCRIBE TABLE lt_rows.
*        IF sy-tfill < 1.
**          MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
*        ELSE.
*          me->pdf( rows = lt_rows ).
*        ENDIF.
    ENDCASE.
*
    gr_alv->refresh( ).

  ENDMETHOD.                    "on_user_command
  METHOD pdf.
*    DATA : fp_outputparams TYPE sfpoutputparams,
*           fp_docparams    TYPE sfpdocparams,
*           fp_formoutput   TYPE fpformoutput,
*           fp_result       TYPE sfpjoboutput,
*           lt_report       TYPE TABLE OF zetpa_s019,
*           ls_report       TYPE zetpa_s019,
*           lv_fm_name      TYPE rs38l_fnam.
*
*    CLEAR lt_report[].
*    LOOP AT rows INTO DATA(ls_row).
*      READ TABLE gt_report INTO DATA(ok_report) INDEX ls_row.
*      MOVE-CORRESPONDING ok_report TO ls_report.
*      APPEND ls_report TO lt_report.
*    ENDLOOP.
*    CHECK lt_report[] IS NOT INITIAL.
*    fp_docparams-langu    = sy-langu.
*    fp_docparams-country  = 'TR'.
*
**    fp_outputparams-nodialog = 'X'.
**    fp_outputparams-getpdf   = 'X'.
*
*    CALL FUNCTION 'FP_JOB_OPEN'
*      CHANGING
*        ie_outputparams = fp_outputparams
*      EXCEPTIONS
*        cancel          = 1
*        usage_error     = 2
*        system_error    = 3
*        internal_error  = 4
*        OTHERS          = 5.
*    CHECK sy-subrc = 0.
*
*    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
*      EXPORTING
*        i_name     = 'ZETPA_AF002'
*      IMPORTING
*        e_funcname = lv_fm_name.
*
*    IF lv_fm_name IS NOT INITIAL .
*      CALL FUNCTION lv_fm_name "'/1BCDWB/SM00000005'
*        EXPORTING
*          /1bcdwb/docparams  = fp_docparams
*          gt_data            = lt_report
*        IMPORTING
*          /1bcdwb/formoutput = fp_formoutput
*        EXCEPTIONS
*          usage_error        = 1
*          system_error       = 2
*          internal_error     = 3
*          OTHERS             = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*    ENDIF .
*
*    CALL FUNCTION 'FP_JOB_CLOSE'
*      IMPORTING
*        e_result       = fp_result
*      EXCEPTIONS
*        usage_error    = 1
*        system_error   = 2
*        internal_error = 3
*        OTHERS         = 4.
*

  ENDMETHOD.                    "pdf
ENDCLASS. "lcl_report IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .
  IF pnpstat2[] IS INITIAL.
    pnpstat2 = 'IEQ3'.
    APPEND pnpstat2.
  ENDIF.
ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
