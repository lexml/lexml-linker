#!/usr/bin/env python3

from argparse import ArgumentParser
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import parse_qs, quote, urlsplit
from urllib.request import Request, urlopen


MAX_PAGE_BYTES = 5 * 1024 * 1024
REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
LEXML_BASE_URL = "https://www.lexml.gov.br/urn/"


class DemoHandler(SimpleHTTPRequestHandler):
    def do_GET(self):
        request_url = urlsplit(self.path)
        if request_url.path == "/lexml-metadata":
            self.proxy_lexml_metadata(request_url.query)
            return
        super().do_GET()

    def proxy_lexml_metadata(self, query):
        urn_values = parse_qs(query).get("urn", [])
        if len(urn_values) != 1 or not valid_document_urn(urn_values[0]):
            self.send_error(400, "Expected one document-level urn:lex: value")
            return

        urn = urn_values[0]
        upstream_url = LEXML_BASE_URL + quote(urn, safe=":;,.-_~")
        request = Request(
            upstream_url,
            headers={
                "Accept": "text/html,application/xhtml+xml",
                "User-Agent": "LexML-Linker-WASM-Demo/1.0",
            },
        )

        try:
            with urlopen(request, timeout=15) as response:
                page = response.read(MAX_PAGE_BYTES + 1)
                if len(page) > MAX_PAGE_BYTES:
                    self.send_error(502, "LexML metadata page is too large")
                    return
        except HTTPError as error:
            self.send_error(502, f"LexML returned HTTP {error.code}")
            return
        except (URLError, TimeoutError) as error:
            reason = getattr(error, "reason", str(error))
            self.send_error(502, f"Could not reach LexML: {reason}")
            return

        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=UTF-8")
        self.send_header("Cache-Control", "public, max-age=3600")
        self.send_header("Content-Length", str(len(page)))
        self.end_headers()
        self.wfile.write(page)


def valid_document_urn(urn):
    return (
        urn.startswith("urn:lex:")
        and "!" not in urn
        and "\r" not in urn
        and "\n" not in urn
        and len(urn) <= 2048
    )


def main():
    parser = ArgumentParser(description="Serve the LexML rich-text WASM demo")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", default=8000, type=int)
    args = parser.parse_args()

    handler = partial(DemoHandler, directory=REPOSITORY_ROOT)
    server = ThreadingHTTPServer((args.host, args.port), handler)
    print(f"Serving http://{args.host}:{args.port}/wasm-rich-demo/")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
