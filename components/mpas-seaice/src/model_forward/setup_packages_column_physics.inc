     ! column physics package packages
     logical, pointer :: &
          config_use_column_package, &
          config_use_column_biogeochemistry

     logical, pointer :: &
          pkgColumnPackageActive, &
          pkgColumnBiogeochemistryActive

     ! column tracers
     logical, pointer :: &
          config_use_ice_age, &
          config_use_first_year_ice, &
          config_use_level_ice, &
          config_use_cesm_meltponds, &
          config_use_level_meltponds, &
          config_use_topo_meltponds, &
          config_use_aerosols, &
          config_use_brine, &
          config_use_vertical_zsalinity, &
          config_use_vertical_biochemistry, &
          config_use_vertical_tracers, &
          config_use_skeletal_biochemistry, &
          config_use_nitrate, &
          config_use_carbon, &
          config_use_chlorophyll, &
          config_use_ammonium, &
          config_use_silicate, &
          config_use_DMS, &
          config_use_nonreactive, &
          config_use_humics, &
          config_use_DON, &
          config_use_iron, &
          config_use_zaerosols, &
          config_use_snow_grain_radius, &
          config_use_effective_snow_density

     logical, pointer :: &
          pkgColumnTracerIceAgeActive, &
          pkgColumnTracerFirstYearIceActive, &
          pkgColumnTracerLevelIceActive, &
          pkgColumnTracerPondsActive, &
          pkgColumnTracerLidThicknessActive, &
          pkgColumnTracerAerosolsActive, &
          pkgTracerBrineActive, &
          pkgTracerMobileFractionActive, &
          pkgTracerSkeletalAlgaeActive, &
          pkgTracerSkeletalNitrateActive, &
          pkgTracerSkeletalCarbonActive, &
          pkgTracerSkeletalAmmoniumActive, &
          pkgTracerSkeletalSilicateActive, &
          pkgTracerSkeletalDMSActive, &
          pkgTracerSkeletalNonreactiveActive, &
          pkgTracerSkeletalHumicsActive, &
          pkgTracerSkeletalDONActive, &
          pkgTracerSkeletalIronActive, &
          pkgTracerVerticalAlgaeActive, &
          pkgTracerVerticalNitrateActive, &
          pkgTracerVerticalCarbonActive, &
          pkgTracerVerticalAmmoniumActive, &
          pkgTracerVerticalSilicateActive, &
          pkgTracerVerticalDMSActive, &
          pkgTracerVerticalNonreactiveActive, &
          pkgTracerVerticalHumicsActive, &
          pkgTracerVerticalDONActive, &
          pkgTracerVerticalIronActive, &
          pkgTracerZAerosolsActive, &
          pkgTracerZSalinityActive, &
          pkgColumnTracerEffectiveSnowDensityActive, &
          pkgColumnTracerSnowGrainRadiusActive

     ! other packages
     logical, pointer :: &
          config_use_form_drag

     logical, pointer :: &
          pkgColumnFormDragActive

     logical :: &
          use_meltponds

     ierr = 0

     !-----------------------------------------------------------------------
     ! column main routines
     !-----------------------------------------------------------------------

     !pkgColumnPackage
     !pkgColumnBiogeochemistry

     call MPAS_pool_get_config(configPool, "config_use_column_package", config_use_column_package)
     call MPAS_pool_get_config(configPool, "config_use_column_biogeochemistry", config_use_column_biogeochemistry)

     call MPAS_pool_get_package(packagePool, "pkgColumnPackageActive", pkgColumnPackageActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnBiogeochemistryActive", pkgColumnBiogeochemistryActive)

     pkgColumnPackageActive         = config_use_column_package
     pkgColumnBiogeochemistryActive = (config_use_column_biogeochemistry .and. config_use_column_package)

     !pkgColumnPackageActive         = .true.
     !pkgColumnBiogeochemistryActive = .true.

     !-----------------------------------------------------------------------
     ! tracers
     !-----------------------------------------------------------------------

     !pkgColumnTracerIceAge
     !pkgColumnTracerFirstYearIce
     !pkgColumnTracerLevelIce
     !pkgColumnTracerPonds
     !pkgColumnTracerLidThickness
     !pkgColumnTracerAerosols
     !pkgTracerBrine
     !pkgTracerMobileFraction
     !pkgTracerSkeletalAlgae
     !pkgTracerSkeletalNitrate
     !pkgTracerSkeletalCarbon
     !pkgTracerSkeletalAmmonium
     !pkgTracerSkeletalSilicate
     !pkgTracerSkeletalDMS
     !pkgTracerSkeletalNonreactive
     !pkgTracerSkeletalHumics
     !pkgTracerSkeletalDON
     !pkgTracerSkeletalIron
     !pkgTracerVerticalAlgae
     !pkgTracerVerticalNitrate
     !pkgTracerVerticalCarbon
     !pkgTracerVerticalAmmonium
     !pkgTracerVerticalSilicate
     !pkgTracerVerticalDMS
     !pkgTracerVerticalNonreactive
     !pkgTracerVerticalHumics
     !pkgTracerVerticalDON
     !pkgTracerVerticalIron
     !pkgTracerZAerosols
     !pkgTracerZSalinity
     !pkgColumnTracerEffectiveSnowDensity
     !pkgColumnTracerSnowGrainRadius

     call MPAS_pool_get_config(configPool, "config_use_ice_age", config_use_ice_age)
     call MPAS_pool_get_config(configPool, "config_use_first_year_ice", config_use_first_year_ice)
     call MPAS_pool_get_config(configPool, "config_use_level_ice", config_use_level_ice)
     call MPAS_pool_get_config(configPool, "config_use_cesm_meltponds", config_use_cesm_meltponds)
     call MPAS_pool_get_config(configPool, "config_use_level_meltponds", config_use_level_meltponds)
     call MPAS_pool_get_config(configPool, "config_use_topo_meltponds", config_use_topo_meltponds)
     call MPAS_pool_get_config(configPool, "config_use_aerosols", config_use_aerosols)
     call MPAS_pool_get_config(configPool, "config_use_brine", config_use_brine)
     call MPAS_pool_get_config(configPool, "config_use_vertical_zsalinity", config_use_vertical_zsalinity)
     call MPAS_pool_get_config(configPool, "config_use_vertical_tracers", config_use_vertical_tracers)
     call MPAS_pool_get_config(configPool, "config_use_skeletal_biochemistry", config_use_skeletal_biochemistry)
     call MPAS_pool_get_config(configPool, "config_use_vertical_biochemistry", config_use_vertical_biochemistry)
     call MPAS_pool_get_config(configPool, "config_use_nitrate", config_use_nitrate)
     call MPAS_pool_get_config(configPool, "config_use_carbon", config_use_carbon)
     call MPAS_pool_get_config(configPool, "config_use_chlorophyll", config_use_chlorophyll)
     call MPAS_pool_get_config(configPool, "config_use_ammonium", config_use_ammonium)
     call MPAS_pool_get_config(configPool, "config_use_silicate", config_use_silicate)
     call MPAS_pool_get_config(configPool, "config_use_DMS", config_use_DMS)
     call MPAS_pool_get_config(configPool, "config_use_nonreactive", config_use_nonreactive)
     call MPAS_pool_get_config(configPool, "config_use_humics", config_use_humics)
     call MPAS_pool_get_config(configPool, "config_use_DON", config_use_DON)
     call MPAS_pool_get_config(configPool, "config_use_iron", config_use_iron)
     call MPAS_pool_get_config(configPool, "config_use_zaerosols", config_use_zaerosols)
     call MPAS_pool_get_config(configPool, "config_use_effective_snow_density", config_use_effective_snow_density)
     call MPAS_pool_get_config(configPool, "config_use_snow_grain_radius", config_use_snow_grain_radius)

     call MPAS_pool_get_package(packagePool, "pkgColumnTracerIceAgeActive", pkgColumnTracerIceAgeActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnTracerFirstYearIceActive", pkgColumnTracerFirstYearIceActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnTracerLevelIceActive", pkgColumnTracerLevelIceActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnTracerPondsActive", pkgColumnTracerPondsActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnTracerLidThicknessActive", pkgColumnTracerLidThicknessActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnTracerAerosolsActive", pkgColumnTracerAerosolsActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerBrineActive", pkgTracerBrineActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerMobileFractionActive", pkgTracerMobileFractionActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalAlgaeActive", pkgTracerSkeletalAlgaeActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalNitrateActive", pkgTracerSkeletalNitrateActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalCarbonActive", pkgTracerSkeletalCarbonActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalAmmoniumActive", pkgTracerSkeletalAmmoniumActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalSilicateActive", pkgTracerSkeletalSilicateActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalDMSActive", pkgTracerSkeletalDMSActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalNonreactiveActive", pkgTracerSkeletalNonreactiveActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalHumicsActive", pkgTracerSkeletalHumicsActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalDONActive", pkgTracerSkeletalDONActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerSkeletalIronActive", pkgTracerSkeletalIronActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalAlgaeActive", pkgTracerVerticalAlgaeActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalNitrateActive", pkgTracerVerticalNitrateActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalCarbonActive", pkgTracerVerticalCarbonActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalAmmoniumActive", pkgTracerVerticalAmmoniumActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalSilicateActive", pkgTracerVerticalSilicateActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalDMSActive", pkgTracerVerticalDMSActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalNonreactiveActive", pkgTracerVerticalNonreactiveActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalHumicsActive", pkgTracerVerticalHumicsActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalDONActive", pkgTracerVerticalDONActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerVerticalIronActive", pkgTracerVerticalIronActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerZAerosolsActive", pkgTracerZAerosolsActive)
     call MPAS_pool_get_package(packagePool, "pkgTracerZSalinityActive", pkgTracerZSalinityActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnTracerEffectiveSnowDensityActive", pkgColumnTracerEffectiveSnowDensityActive)
     call MPAS_pool_get_package(packagePool, "pkgColumnTracerSnowGrainRadiusActive", pkgColumnTracerSnowGrainRadiusActive)

     use_meltponds = (config_use_cesm_meltponds .or. config_use_level_meltponds .or. config_use_topo_meltponds)

     pkgColumnTracerIceAgeActive       = config_use_ice_age
     pkgColumnTracerFirstYearIceActive = config_use_first_year_ice
     pkgColumnTracerLevelIceActive     = config_use_level_ice
     pkgColumnTracerPondsActive        = use_meltponds
     pkgColumnTracerLidThicknessActive = (config_use_level_meltponds .or. config_use_topo_meltponds)
     pkgColumnTracerAerosolsActive     = config_use_aerosols

     pkgTracerBrineActive                = config_use_brine
     pkgTracerMobileFractionActive       = config_use_vertical_tracers
     pkgTracerSkeletalAlgaeActive        = config_use_skeletal_biochemistry
     pkgTracerSkeletalNitrateActive      = (config_use_skeletal_biochemistry .and. config_use_nitrate)
     pkgTracerSkeletalCarbonActive       = (config_use_skeletal_biochemistry .and. config_use_carbon)
     pkgTracerSkeletalAmmoniumActive     = (config_use_skeletal_biochemistry .and. config_use_ammonium)
     pkgTracerSkeletalSilicateActive     = (config_use_skeletal_biochemistry .and. config_use_silicate)
     pkgTracerSkeletalDMSActive          = (config_use_skeletal_biochemistry .and. config_use_DMS)
     pkgTracerSkeletalNonreactiveActive  = (config_use_skeletal_biochemistry .and. config_use_nonreactive)
     pkgTracerSkeletalHumicsActive       = (config_use_skeletal_biochemistry .and. config_use_humics)
     pkgTracerSkeletalDONActive          = (config_use_skeletal_biochemistry .and. config_use_DON)
     pkgTracerSkeletalIronActive         = (config_use_skeletal_biochemistry .and. config_use_iron)
     pkgTracerVerticalAlgaeActive        = (config_use_vertical_tracers .and. config_use_vertical_biochemistry)
     pkgTracerVerticalNitrateActive      = (config_use_vertical_tracers .and. config_use_nitrate)
     pkgTracerVerticalCarbonActive       = (config_use_vertical_tracers .and. config_use_carbon)
     pkgTracerVerticalAmmoniumActive     = (config_use_vertical_tracers .and. config_use_ammonium)
     pkgTracerVerticalSilicateActive     = (config_use_vertical_tracers .and. config_use_silicate)
     pkgTracerVerticalDMSActive          = (config_use_vertical_tracers .and. config_use_DMS)
     pkgTracerVerticalNonreactiveActive  = (config_use_vertical_tracers .and. config_use_nonreactive)
     pkgTracerVerticalHumicsActive       = (config_use_vertical_tracers .and. config_use_humics)
     pkgTracerVerticalDONActive          = (config_use_vertical_tracers .and. config_use_DON)
     pkgTracerVerticalIronActive         = (config_use_vertical_tracers .and. config_use_iron)
     pkgTracerZAerosolsActive            = config_use_zaerosols
     pkgTracerZSalinityActive            = config_use_vertical_zsalinity

     pkgColumnTracerEffectiveSnowDensityActive = config_use_effective_snow_density
     pkgColumnTracerSnowGrainRadiusActive = config_use_snow_grain_radius

     if (.not. config_use_column_package) then
        pkgColumnTracerIceAgeActive       = .false.
        pkgColumnTracerFirstYearIceActive = .false.
        pkgColumnTracerLevelIceActive     = .false.
        pkgColumnTracerPondsActive        = .false.
        pkgColumnTracerLidThicknessActive = .false.
        pkgColumnTracerAerosolsActive     = .false.
        pkgTracerBrineActive                = .false.
        pkgTracerMobileFractionActive       = .false.
        pkgTracerSkeletalAlgaeActive        = .false.
        pkgTracerSkeletalNitrateActive      = .false.
        pkgTracerSkeletalCarbonActive       = .false.
        pkgTracerSkeletalAmmoniumActive     = .false.
        pkgTracerSkeletalSilicateActive     = .false.
        pkgTracerSkeletalDMSActive          = .false.
        pkgTracerSkeletalNonreactiveActive  = .false.
        pkgTracerSkeletalHumicsActive       = .false.
        pkgTracerSkeletalDONActive          = .false.
        pkgTracerSkeletalIronActive         = .false.
        pkgTracerVerticalAlgaeActive        = .false.
        pkgTracerVerticalNitrateActive      = .false.
        pkgTracerVerticalCarbonActive       = .false.
        pkgTracerVerticalAmmoniumActive     = .false.
        pkgTracerVerticalSilicateActive     = .false.
        pkgTracerVerticalDMSActive          = .false.
        pkgTracerVerticalNonreactiveActive  = .false.
        pkgTracerVerticalHumicsActive       = .false.
        pkgTracerVerticalDONActive          = .false.
        pkgTracerVerticalIronActive         = .false.
        pkgTracerZAerosolsActive            = .false.
        pkgTracerZSalinityActive            = .false.
        pkgColumnTracerEffectiveSnowDensityActive = .false.
        pkgColumnTracerSnowGrainRadiusActive      = .false.
     endif

     if (.not. config_use_column_biogeochemistry .and. config_use_column_package) then
        pkgTracerBrineActive                = .false.
        pkgTracerMobileFractionActive       = .false.
        pkgTracerSkeletalAlgaeActive        = .false.
        pkgTracerSkeletalNitrateActive      = .false.
        pkgTracerSkeletalCarbonActive       = .false.
        pkgTracerSkeletalAmmoniumActive     = .false.
        pkgTracerSkeletalSilicateActive     = .false.
        pkgTracerSkeletalDMSActive          = .false.
        pkgTracerSkeletalNonreactiveActive  = .false.
        pkgTracerSkeletalHumicsActive       = .false.
        pkgTracerSkeletalDONActive          = .false.
        pkgTracerSkeletalIronActive         = .false.
        pkgTracerVerticalAlgaeActive        = .false.
        pkgTracerVerticalNitrateActive      = .false.
        pkgTracerVerticalCarbonActive       = .false.
        pkgTracerVerticalAmmoniumActive     = .false.
        pkgTracerVerticalSilicateActive     = .false.
        pkgTracerVerticalDMSActive          = .false.
        pkgTracerVerticalNonreactiveActive  = .false.
        pkgTracerVerticalHumicsActive       = .false.
        pkgTracerVerticalDONActive          = .false.
        pkgTracerVerticalIronActive         = .false.
        pkgTracerZAerosolsActive            = .false.
        pkgTracerZSalinityActive            = .false.
     endif

     !pkgColumnTracerIceAgeActive       = .true.
     !pkgColumnTracerFirstYearIceActive = .true.
     !pkgColumnTracerLevelIceActive     = .true.
     !pkgColumnTracerPondsActive        = .true.
     !pkgColumnTracerLidThicknessActive = .true.
     !pkgColumnTracerAerosolsActive     = .true.
     !pkgColumnTracerArrayStandInActive = .true.

     !-----------------------------------------------------------------------
     ! other column packages
     !-----------------------------------------------------------------------

     !pkgColumnFormDrag

     ! form drag
     call MPAS_pool_get_config(configPool, "config_use_form_drag", config_use_form_drag)
     call MPAS_pool_get_package(packagePool, "pkgColumnFormDragActive", pkgColumnFormDragActive)
     pkgColumnFormDragActive = (config_use_column_package .and. config_use_form_drag)

     !pkgColumnFormDragActive = .true.
