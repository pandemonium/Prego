package se.jwzrd.prego.core.server

import http.{Response, Request}

/**
 * @author Patrik Andersson <pandersson@gmail.com>
 */
package object http {
  type RequestProcessor = Request => Response

  
}