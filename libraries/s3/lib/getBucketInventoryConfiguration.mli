open Types
type input = GetBucketInventoryConfigurationRequest.t
type output = GetBucketInventoryConfigurationOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error