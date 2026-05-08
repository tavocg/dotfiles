# shellcheck shell=sh

case ":$LD_LIBRARY_PATH:" in
*":/run/opengl-driver/lib:"*) ;;
*) export LD_LIBRARY_PATH="/run/opengl-driver/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}" ;;
esac
