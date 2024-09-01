#!/usr/bin/env bash

# https://github.com/purescript/spago/blob/5efaad1329e2c47830f2fc7e491a0765d1f956d4/src/Spago/Command/Run.purs#L74-L125

# Define paths
local_cache_path=$(realpath ./output-es)   # Update this path to your local cache
run_dir="${local_cache_path}/run"
run_js_path="${run_dir}/run.js"
package_json_path="${run_dir}/package.json"
package_json_contents='{"type":"module"}'

# Create directory if it doesn't exist
mkdir -p "${run_dir}"

# Node arguments and content to be written to run.js
module_name="$1"  # First argument (updated to $1)
shift  # Shift arguments to get the rest
exec_args=("$@")  # Capture all remaining arguments

node_args=("${run_js_path}" "${exec_args[@]}")
node_contents=$(cat <<EOF
import { main } from 'file://${local_cache_path}/${module_name}/index.js'

main()
EOF
)

# Write the node contents to run.js
echo "Writing ${run_js_path}"
echo "${node_contents}" > "${run_js_path}"
chmod +x "${run_js_path}"

# Write the package.json content
echo "Writing ${package_json_path}"
echo "${package_json_contents}" > "${package_json_path}"

# Define execute directory, or set a default if not defined
execute_dir="${execute_dir:-$(pwd)}"

# Log the execution directory and node command
echo "Executing from: ${execute_dir}"
echo "Running node command with args: ${node_args[@]}"

# Execute the node command
pushd "${execute_dir}" > /dev/null
node "${node_args[@]}"
exec_result=$?
popd > /dev/null

# Handle success or failure based on the result
if [[ $exec_result -eq 0 ]]; then
  if [[ -n "${success_message}" ]]; then
    echo "${success_message}"
  fi
else
  echo "${failure_message}"
  exit 1
fi
