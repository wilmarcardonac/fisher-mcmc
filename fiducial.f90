Module fiducial

    Implicit none

    save 

    !#############################
    ! PARAMETERS OF FIDUCIAL MODEL
    !#############################

    Real*8,parameter :: omega_b = 2.225d-2
    Real*8,parameter :: omega_cdm = 1.198d-1
    Real*8,parameter :: n_s = 9.645d-1
    Real*8,parameter :: A_s = 2.20652d-9
    Real*8,parameter :: H0 = 6.727d1
    Real*8,parameter :: m_ncdm = 6.0d-2
    Real*8,parameter :: MG_beta2 = 1.00d0
    Real*8,parameter :: N_ur = 0.d0
    Real*8,parameter :: N_ncdm = 1.d0
    Real*8,parameter :: deg_ncdm = 3.d0
    Real*8,parameter :: tau = 0.089d0

    Character(len=*),parameter :: param_name_omega_b = 'omega_b'
    Character(len=*),parameter :: param_name_omega_cdm = 'omega_cdm'
    Character(len=*),parameter :: param_name_n_s = 'n_s'
    Character(len=*),parameter :: param_name_A_s = 'A_s'
    Character(len=*),parameter :: param_name_H0 = 'H0'
    Character(len=*),parameter :: param_name_m_ncdm = 'm_ncdm'
    Character(len=*),parameter :: param_name_MG_beta2 = 'MG_beta2'

    !################################################
    ! 1-SIGMA VALUES FOR PARAMETERS IN FIDUCIAL MODEL
    !################################################

    Real*8,parameter :: sigma_omega_b = 1.6d-4
    Real*8,parameter :: sigma_omega_cdm = 1.5d-3
    Real*8,parameter :: sigma_n_s = 4.9d-3
    Real*8,parameter :: sigma_A_s = 1.541d-11
    Real*8,parameter :: sigma_H0 = 6.6d-1
    Real*8,parameter :: sigma_m_ncdm = 5.d-3
    Real*8,parameter :: sigma_MG_beta2 = 2.5d-1

    !################################
    ! CLASS AND SURVEY SPECIFICATIONS
    !################################

    Integer*4,parameter :: nbins = 5
    Integer*4,parameter :: lmax_class = 2000
    Integer*4,parameter :: lmin = 2

    Real*8,parameter    :: zmin = 0.1d0
    Real*8,parameter    :: zmax = 2.0d0
    Real*8,parameter    :: dz = 1.0d-3
    Real*8,parameter    :: gal_per_sqarcmn = 30.d0
    Real*8,parameter    :: Pi = 3.141592653589793d0
    Real*8,parameter    :: fsky = 1.5d4/4.1253d4
    Real*8,parameter    :: theoreticalerror = 0.d0 !5.d-2
    Real*8,parameter    :: l_switch_limber_for_cl_density_over_z = 20000.d0
    Real*8,parameter    :: selection_sampling_bessel_fid = 3.d0
    Real*8,parameter    :: q_linstep_fid = 0.3d0
    Real*8,parameter    :: k_max_tau0_over_l_max_fid = 20.d0

    !##################
    ! FISHER PARAMETERS
    !##################

    Integer*4,parameter :: n_points = 5 ! NUMBER OF POINTS PER COSMOLOGICAL PARAMETER
    Integer*4,parameter :: lmax = 400   ! HIGHEST MULTIPOLE

    Logical,parameter   :: compute_data_fisher_analysis = .false.   ! COMPUTE DATA FOR FISHER ANALYSIS IF SET IT TRUE
    Logical,parameter   :: do_fisher_analysis = .true. ! DO FISHER MATRIX ANALYSIS IF SET IT TRUE
    Logical,parameter   :: testing_precision = .false. ! PERFORM PRECISION TEST IF SET IT TRUE
    Logical,parameter   :: compute_data_testing_precision = .false. ! COMPUTE DATA FOR PRECISION TEST IF SET IT TRUE

    !################
    ! MCMC PARAMETERS
    !################

    Integer*4,parameter    :: number_iterations = 100000        ! TOTAL NUMBER OF ITERATIONS IN MCMC RUN
    Integer*4,parameter    :: number_of_parameters = 7       ! NUMBER OF COSMOLOGICAL PARAMETERS
    Integer*4,parameter    :: jumping_factor_update = 100    ! STEPS TAKEN BEFORE UPDATING JUMPING FACTOR (IF NEEDED)
    Integer*4,parameter    :: covariance_matrix_update = 10000 ! STEPS TAKEN BEFORE UPDATING COVARIANCE MATRIX (IF NEEDED)
    Integer*4,parameter    :: steps_taken_before_definite_run = 10000 ! STEPS TAKEN BEFORE FREEZING COVARIANCE MATRIX

    Real*8,parameter       :: step_size_changes = 1.d-1      ! CHANGE IN STEP SIZE

    Character*16,parameter :: phrase = 'randomizer'       ! PHRASE NEEDED BY NUMBER RANDOM GENERATOR

    Logical,parameter      :: using_inverse_fisher_matrix = .false. !  USE INVERSE OF FISHER MATRIX AS A COVARIANCE MATRIX IF SET IT TRUE  
    Logical,parameter      :: do_mcmc_analysis = .false.    ! DO MCMC ANALYSIS IF SET IT TRUE
    Logical,parameter      :: start_from_fiducial = .false.    ! START MCMC ANALYSIS FROM FIDUCIAL POINT IF SET IT TRUE
    Logical,parameter      :: testing_Gaussian_likelihood = .false.  ! TEST GAUSSIAN LIKELIHOOD IF SET IT TRUE

    !###############
    ! PATHS TO FILES
    !###############

    Character(len=*),parameter :: Execution_information = './output/execution_information.txt'
    
End Module fiducial
