FUNCTION-POOL ZHRBR_FG001.                  "MESSAGE-ID ..

* INCLUDE LZHRIC_FG001D...                   " Local class definition
TYPE-POOLS: soi.
INCLUDE ole2incl.
DATA: gs_find    TYPE ole2_object,
      gs_replace TYPE ole2_object,
      gs_range   TYPE ole2_object,
      gs_footer  TYPE ole2_object.
DATA : found           TYPE i.

"--->add code  by MKARABULUT ~ begin
DATA : gv_url(200) TYPE c.
DATA : gv_flag TYPE i.
DATA: selection        TYPE ole2_object.
DATA: inlineshapes     TYPE ole2_object.
"--<<add code  by MKARABULUT ~ end

*PARAMETERS :  p_fname LIKE rlgrap-filename OBLIGATORY.

DATA : BEGIN OF itab OCCURS 0,
         sa TYPE string,
       END OF itab.
DATA : fname TYPE string.


DATA :  buffer_t TYPE TABLE OF xstring.
DATA : w_buffer LIKE LINE OF buffer_t.
DATA : l_id(8).
DATA :    file_detail TYPE sapb-sapfiles.
DATA : ls_data TYPE solix.
DATA : i_data  TYPE solix_tab.

DATA : gt_value TYPE TABLE OF zhrbr_s002,
       gs_value LIKE LINE OF gt_value.


DATA : lv_len       TYPE i,
       lv_lines     TYPE i,
       lv_xstring   TYPE xstring,
       lv_line(255).

DATA: buffer    TYPE xstring,
      path_name TYPE dirname_al11,
      stnglen   TYPE i.

DATA : strlen      LIKE  sy-fdpos.
DATA:  max_len_of_filename      TYPE i VALUE  260.

DATA  list_filename TYPE dirname_al11.

DATA : BEGIN OF gv,
         classname  TYPE  sbdst_classname,
         classtype  TYPE  sbdst_classtype,
         object_key TYPE  sbdst_object_key,
       END OF   gv.


DATA : gt_tabname TYPE TABLE OF tabname.

DEFINE m_add.
  APPEND &1 TO gt_tabname.
END-OF-DEFINITION.
