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
struct UnitWrap::UnitTest<D>::TestIopBroadcast {

  static void run_bfb()
  {
    auto engine = setup_random_test();

    IopBroadcastData f90_data[max_pack_size]; //= {
    //   // TODO
    // };

    static constexpr Int num_runs = sizeof(f90_data) / sizeof(IopBroadcastData);

    // Generate random input data
    // Alternatively, you can use the f90_data construtors/initializer lists to hardcode data
    for (auto& d : f90_data) {
      d.randomize(engine);
    }

#if 0
    // Create copies of data for use by cxx and sync it to device. Needs to happen before fortran calls so that
    // inout data is in original state
    view_1d<IopBroadcastData> cxx_device("cxx_device", max_pack_size);
    const auto cxx_host = Kokkos::create_mirror_view(cxx_device);
    std::copy(&f90_data[0], &f90_data[0] + max_pack_size, cxx_host.data());
    Kokkos::deep_copy(cxx_device, cxx_host);

    // Get data from fortran
    for (auto& d : f90_data) {
      iop_broadcast(d);
    }

    // Get data from cxx. Run iop_broadcast from a kernel and copy results back to host
    // Kokkos::parallel_for(num_test_itrs, KOKKOS_LAMBDA(const Int& i) {
    //   const Int offset = i * Spack::n;

      
      

    //   Functions::iop_broadcast();

      
    // });

    Kokkos::deep_copy(cxx_host, cxx_device);

    // Verify BFB results
    if (SCREAM_BFB_TESTING) {
      for (Int i = 0; i < num_runs; ++i) {
        IopBroadcastData& d_f90 = f90_data[i];
        IopBroadcastData& d_cxx = cxx_host[i];
      }
    }
#endif
  } // run_bfb
};

} // namespace unit_test
} // namespace dp
} // namespace scream

namespace {

TEST_CASE("iop_broadcast_bfb", "[dp]")
{
  using TestStruct = scream::dp::unit_test::UnitWrap::UnitTest<scream::DefaultDevice>::TestIopBroadcast;

  TestStruct::run_bfb();
}

} // empty namespace
