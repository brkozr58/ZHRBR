*&---------------------------------------------------------------------*
*& Include          ZETPA_P029_I004
*&---------------------------------------------------------------------*
INITIALIZATION.
  CREATE OBJECT gr_report.
  gr_report->set_init( ).

START-OF-SELECTION.

  gr_report->set_date( ).

  rp-set-data-interval 'P9950' s_datum-low s_datum-high.
GET pernr.
*
  rp_provide_from_last p0000 space s_datum-low s_datum-high.
  rp_provide_from_last p0001 space s_datum-low s_datum-high.
  rp_provide_from_last p0041 space s_datum-low s_datum-high.
  rp_provide_from_last p9950 space s_datum-low s_datum-high.
  rp-set-data-interval 'P9951' s_datum-low s_datum-high.
*
  gr_report->get_person( ).
*
END-OF-SELECTION.

  gr_report->prepare_alv( ).
