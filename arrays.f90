Module arrays

    Integer :: status1,status2,status3,status4,status5,status6
    Integer*4,dimension(13) :: buff

    Real*8, allocatable, dimension(:) :: param_omega_b, param_omega_cdm, param_n_s, param_A_s,old_point,bestfit
    Real*8, allocatable, dimension(:) :: param_H0,param_m_ncdm,B_beta,b_lambda,param_MG_beta2,current_point,means
    Real*8, allocatable, dimension(:,:,:) :: cov_l_IP,inv_cov_l_IP,cov_l_IP_oa,inv_cov_l_IP_oa
    Real*8, allocatable, dimension(:,:,:,:,:) :: cov,inv_cov,inv_cov_oa
    Real*8, allocatable, dimension(:,:,:) :: El, Cl_fid, Cl_fid_nl, Cl_obs,Cl_1, Cl_2, Cl_3, Cl_4, dCl,d1,d2,d3,d4,d5,d6,d7
    Real*8, allocatable, dimension(:,:,:) :: Cl_5, Cl_6, Cl_7, Cl_8, dCl_nl,Cl_syst,Cl_current
    Real*8, allocatable, dimension(:,:) :: Nl,F_ab,F_ab_nl,inv_F_ab
    Real*4, allocatable, dimension(:) :: acceptance_probability

End module arrays
