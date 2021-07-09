{ mkDerivation, aeson, base, bytestring, fetchgit, hspec
, http-client, http-types, lib, process, prometheus-client, servant
, servant-client, servant-server, text, time, transformers
, unordered-containers, wai, warp
}:
mkDerivation {
  pname = "servant-prometheus";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/PrivateStorageio/servant-prometheus.git";
    sha256 = "1gfslw670ri119bnq3szc8b08n504f8cnzs5cgk5qvfwvfmsr1xh";
    rev = "b9461cbf689b47506b2eee973136706092b74968";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring http-types prometheus-client servant text time
    unordered-containers wai
  ];
  executableHaskellDepends = [
    aeson base process prometheus-client servant-server text wai warp
  ];
  testHaskellDepends = [
    aeson base hspec http-client prometheus-client servant
    servant-client servant-server text transformers
    unordered-containers wai warp
  ];
  description = "Helpers for using prometheus with servant";
  license = lib.licenses.bsd3;
}
