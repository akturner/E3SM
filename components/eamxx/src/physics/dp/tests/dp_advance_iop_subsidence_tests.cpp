#include "catch2/catch.hpp"

#include "share/scream_types.hpp"
#include "ekat/ekat_pack.hpp"
#include "ekat/kokkos/ekat_kokkos_utils.hpp"
#include "physics/dp/dp_functions.hpp"
#include "physics/dp/dp_functions_f90.hpp"

#include "dp_unit_tests_common.hpp"

namespace scream {
namespace dp {
namespace unit_test {

template <typename D>
struct UnitWrap::UnitTest<D>::TestAdvanceIopSubsidence {

  static void run_bfb()
  {
    auto engine = setup_random_test();

    AdvanceIopSubsidenceData f90_data[] = {
      // TODO
    };

    static constexpr Int num_runs = sizeof(f90_data) / sizeof(AdvanceIopSubsidenceData);

    // Generate random input data
    // Alternatively, you can use the f90_data construtors/initializer lists to hardcode data
    for (auto& d : f90_data) {
      d.randomize(engine);
    }

    // Create copies of data for use by cxx. Needs to happen before fortran calls so that
    // inout data is in original state
    AdvanceIopSubsidenceData cxx_data[] = {
      // TODO
    };

    // Assume all data is in C layout

    // Get data from fortran
    for (auto& d : f90_data) {
      // expects data in C layout
      advance_iop_subsidence(d);
    }

    // Get data from cxx
    for (auto& d : cxx_data) {
      d.transpose<ekat::TransposeDirection::c2f>(); // _f expects data in fortran layout
      advance_iop_subsidence_f(d.plev, d.pcnst, d.scm_dt, d.ps_in, d.u_in, d.v_in, d.t_in, d.q_in, d.u_update, d.v_update, d.t_update, d.q_update);
      d.transpose<ekat::TransposeDirection::f2c>(); // go back to C layout
    }

    // Verify BFB results, all data should be in C layout
    if (SCREAM_BFB_TESTING) {
      for (Int i = 0; i < num_runs; ++i) {
        AdvanceIopSubsidenceData& d_f90 = f90_data[i];
        AdvanceIopSubsidenceData& d_cxx = cxx_data[i];
        for (Int k = 0; k < d_f90.total(d_f90.u_update); ++k) {
          REQUIRE(d_f90.total(d_f90.u_update) == d_cxx.total(d_cxx.u_update));
          REQUIRE(d_f90.u_update[k] == d_cxx.u_update[k]);
          REQUIRE(d_f90.total(d_f90.u_update) == d_cxx.total(d_cxx.v_update));
          REQUIRE(d_f90.v_update[k] == d_cxx.v_update[k]);
          REQUIRE(d_f90.total(d_f90.u_update) == d_cxx.total(d_cxx.t_update));
          REQUIRE(d_f90.t_update[k] == d_cxx.t_update[k]);
        }
        for (Int k = 0; k < d_f90.total(d_f90.q_update); ++k) {
          REQUIRE(d_f90.total(d_f90.q_update) == d_cxx.total(d_cxx.q_update));
          REQUIRE(d_f90.q_update[k] == d_cxx.q_update[k]);
        }

      }
    }
  } // run_bfb

};

} // namespace unit_test
} // namespace dp
} // namespace scream

namespace {

TEST_CASE("advance_iop_subsidence_bfb", "[dp]")
{
  using TestStruct = scream::dp::unit_test::UnitWrap::UnitTest<scream::DefaultDevice>::TestAdvanceIopSubsidence;

  TestStruct::run_bfb();
}

} // empty namespace
