#include "catch2/catch.hpp"

//#include "share/scream_types.hpp"
#include <algorithm>
#include <array>
#include <random>
#include <thread>

#include "ekat/scream_kokkos.hpp"
#include "ekat/scream_pack.hpp"
#include "ekat/scream_types.hpp"
#include "ekat/util/scream_arch.hpp"
#include "ekat/util/scream_kokkos_utils.hpp"
#include "ekat/util/scream_utils.hpp"
#include "physics/share/physics_constants.hpp"
#include "physics/shoc/shoc_functions.hpp"
#include "physics/shoc/shoc_functions_f90.hpp"
#include "shoc_unit_tests_common.hpp"

namespace scream {
namespace shoc {
namespace unit_test {

template <typename D>
struct UnitWrap::UnitTest<D>::TestShocEnergyThreshFixer {

  static void run_property()
  {
    static constexpr Int shcol    = 2;
    static constexpr Int nlev     = 5;
    static constexpr auto nlevi   = nlev + 1;

    // Tests for the SHOC function
    //     shoc_energy_threshold_fixer

    // TEST ONE
    // Set up a reasonable profile verify results are as expected

    // Host model TKE.  currently these values are dimensionless and
    //  will be later multipled by tke_min value to get profile in [m2/s2]
    Real tke_input[nlev] = {1.0, 1.0, 10.0, 40.0, 50.0};
    //  Pressure at interface [hPa] (later converted to Pa)
    Real pint[nlevi] = {500.0, 600.0, 700.0, 800.0, 900.0, 1000.0};

    // Integrated total energy after SHOC.
    static constexpr Real te_a = 100.0;
    // Integrated total energy before SHOC
    static constexpr Real te_b = 110.0;

    // Tke minimum value
    static constexpr Real tke_min = 0.0004;
    
    // convert pressure to Pa
    for(Int n = 0; n < nlevi; ++n) {
      pint[n] = 100.0*pint[n];
    }
    
    // convert TKE from dimensionaless to [m2/s2]
    for(Int n = 0; n < nlev; ++n) {
      tke_input[n] = tke_min*tke_input[n];
    }

    // Initialzie data structure for bridgeing to F90
    SHOCEnergythreshfixerData SDS(shcol, nlev, nlevi);

    // Test that the inputs are reasonable.
    REQUIRE(SDS.shcol > 1);
    REQUIRE(SDS.nlev+1 == SDS.nlevi);

    // Fill in test data on zt_grid.
    for(Int s = 0; s < SDS.shcol; ++s) {
      SDS.te_a[s] = te_a;
      SDS.te_b[s] = te_b;
      for(Int n = 0; n < SDS.nlev; ++n) {
	const auto offset = n + s * SDS.nlev;

	SDS.tke[offset] = tke_input[n];
      }
      
      for(Int n = 0; n < SDS.nlevi; ++n) {
	const auto offset = n + s * SDS.nlevi;

	SDS.pint[offset] = pint[n];
      }      
    }

    // Check that the inputs make sense

    for(Int s = 0; s < SDS.shcol; ++s) {
      for (Int n = 0; n < SDS.nlev; ++n){
	const auto offset = n + s * SDS.nlev;

	REQUIRE(SDS.tke[offset] >= tke_min);
      }
    }

    // Call the fortran implementation
    shoc_energy_threshold_fixer(SDS);
    
    // Verify the result
    for(Int s = 0; s < SDS.shcol; ++s) {
      // Make sure value of shoctop is within reasonable range
      REQUIRE(SDS.shoctop[s] < nlev);
      REQUIRE(SDS.shoctop[s] > 1);
      
      // Verify that shoctop represents what we want it to
      // Make sure that thickness that bounds shoctop is positive
      const auto offset_stopi = (SDS.shoctop[s]-1) + s * SDS.nlevi;
      const auto offset_bot = (nlevi-1) + s * SDS.nlevi;
      REQUIRE(SDS.pint[offset_bot] - SDS.pint[offset_stopi] > 0.0);

      if (SDS.shoctop[s] < nlev){
        const auto offset_stop = (SDS.shoctop[s]-1) + s * SDS.nlev;     
        REQUIRE(SDS.tke[offset_stop] == tke_min);
        REQUIRE(SDS.tke[offset_stop+1] > tke_min);
      }
    } 
      
  }

};

}  // namespace unit_test
}  // namespace shoc
}  // namespace scream

namespace {

TEST_CASE("shoc_energy_threshold_fixer_property", "shoc")
{
  using TestStruct = scream::shoc::unit_test::UnitWrap::UnitTest<scream::DefaultDevice>::TestShocEnergyThreshFixer;

  TestStruct::run_property();
}

TEST_CASE("shoc_energy_threshold_fixer_b4b", "shoc")
{
  using TestStruct = scream::shoc::unit_test::UnitWrap::UnitTest<scream::DefaultDevice>::TestShocEnergyThreshFixer;

}

} // namespace
