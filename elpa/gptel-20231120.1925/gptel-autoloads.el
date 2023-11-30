;;; gptel-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from gptel.el

(autoload 'gptel-mode "gptel" "\
Minor mode for interacting with ChatGPT.

This is a minor mode.  If called interactively, toggle the `GPTel
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `gptel-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(autoload 'gptel-send "gptel" "\
Submit this prompt to ChatGPT.

With prefix arg ARG activate a transient menu with more options
instead.

(fn &optional ARG)" t)
(autoload 'gptel "gptel" "\
Switch to or start ChatGPT session with NAME.

With a prefix arg, query for a (new) session name.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt. Returns the
buffer created or switched to.

(fn NAME &optional _ INITIAL)" t)
(register-definition-prefixes "gptel" '("gptel-"))


;;; Generated autoloads from gptel-curl.el

(autoload 'gptel-curl-get-response "gptel-curl" "\
Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point.

(fn INFO &optional CALLBACK)")
(register-definition-prefixes "gptel-curl" '("gptel-"))


;;; Generated autoloads from gptel-ollama.el

(autoload 'gptel-make-ollama "gptel-ollama" "\
Register an Ollama backend for gptel with NAME.

Keyword arguments:

HOST is where Ollama runs (with port), typically localhost:11434

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, http by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/generate\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like Ollama.

Example:
-------

(gptel-make-ollama
  \"Ollama\"
  :host \"localhost:11434\"
  :models \\='(\"mistral:latest\")
  :stream t)

(fn NAME &key HOST HEADER KEY MODELS STREAM (PROTOCOL \"http\") (ENDPOINT \"/api/generate\"))")
(register-definition-prefixes "gptel-ollama" '("gptel--ollama-context"))


;;; Generated autoloads from gptel-openai.el

(autoload 'gptel-make-openai "gptel-openai" "\
Register a ChatGPT backend for gptel with NAME.

Keyword arguments:

HOST (optional) is the API host, typically \"api.openai.com\".

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

(fn NAME &key HEADER MODELS STREAM (KEY \\='gptel-api-key) (HOST \"api.openai.com\") (PROTOCOL \"https\") (ENDPOINT \"/v1/chat/completions\"))")
(autoload 'gptel-make-azure "gptel-openai" "\
Register an Azure backend for gptel with NAME.

Keyword arguments:

HOST is the API host.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

(gptel-make-azure
 \"Azure-1\"
 :protocol \"https\"
 :host \"RESOURCE_NAME.openai.azure.com\"
 :endpoint
 \"/openai/deployments/DEPLOYMENT_NAME/completions?api-version=2023-05-15\"
 :stream t
 :models \\='(\"gpt-3.5-turbo\" \"gpt-4\"))

(fn NAME &key HOST (PROTOCOL \"https\") (HEADER (lambda nil \\=`((\"api-key\" \\=\\, (gptel--get-api-key))))) (KEY \\='gptel-api-key) MODELS STREAM ENDPOINT)")
(defalias 'gptel-make-gpt4all 'gptel-make-openai "\
Register a GPT4All backend for gptel with NAME.

Keyword arguments:

HOST is where GPT4All runs (with port), typically localhost:8491

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v1/completions\"

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like GPT4All.

Example:
-------

(gptel-make-gpt4all
 \"GPT4All\"
 :protocol \"http\"
 :host \"localhost:4891\"
 :models \\='(\"mistral-7b-openorca.Q4_0.gguf\"))")


;;; Generated autoloads from gptel-transient.el

 (autoload 'gptel-menu "gptel-transient" nil t)
(register-definition-prefixes "gptel-transient" '("gptel-"))

;;; End of scraped data

(provide 'gptel-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; gptel-autoloads.el ends here
