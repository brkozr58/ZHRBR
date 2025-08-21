FUNCTION ZHRBR_FG001_003.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_KEY) TYPE  CHAR18
*"  EXPORTING
*"     VALUE(DCMNT_DRM) TYPE  I
*"--------------------------------------------------------------------


  DATA : BEGIN OF kt_gos OCCURS 0,
         instid LIKE sibflporb-instid ,
       END OF kt_gos.

  DATA : gt_gos LIKE kt_gos OCCURS 0 WITH HEADER LINE.
*Attach
  DATA : objec_key          TYPE sibflporb          ,
         it_objects         TYPE sibflporb OCCURS 0 ,
         ls_stat            TYPE sgs_t_acnt         .

  objec_key-instid    = i_key      .
  objec_key-typeid    = 'ZHRBR'  .
  objec_key-catid     = 'BO'       .

  condense objec_key-instid NO-GAPS.

  CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
    EXPORTING
      is_object      = objec_key
      ip_check_arl   = space
      ip_check_bds   = space
      ip_notes       = 'X'
      ip_attachments = 'X'
      ip_urls        = space
      ip_mode        = 'E'
    TABLES
      it_objects     = it_objects
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.
*  CALL METHOD cl_gos_attachment_query=>count_for_object
*    EXPORTING
*      is_object  = objec_key
*      ip_atta    = 'X'
*      ip_note    = 'X'
*      ip_url     = ''
*      ip_arl     = ''
*      ip_private = 'X'
*    RECEIVING
*      rt_stat    = ls_stat.


ENDFUNCTION.
