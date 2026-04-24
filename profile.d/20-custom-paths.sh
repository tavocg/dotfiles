# shellcheck shell=sh

case ":$PATH:" in
*":$HOME/.local/bin:"*) ;;
*) export PATH="$HOME/.local/bin${PATH:+:${PATH}}" ;;
esac

case ":$PATH:" in
*":$HOME/.config/scripts:"*) ;;
*) export PATH="$HOME/.config/scripts${PATH:+:${PATH}}" ;;
esac

case ":$PATH:" in
*":$HOME/.config/wrappers:"*) ;;
*) export PATH="$HOME/.config/wrappers${PATH:+:${PATH}}" ;;
esac

case ":$LD_LIBRARY_PATH:" in
*":$HOME/.local/lib:"*) ;;
*) export LD_LIBRARY_PATH="$HOME/.local/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}" ;;
esac
