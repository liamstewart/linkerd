package io.buoyant.linkerd.admin

import com.twitter.finagle.{Dtab, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Future
import io.buoyant.linkerd.Linker
import io.buoyant.linkerd.admin.names.WebDelegator
import io.buoyant.router.RoutingFactory

private object DelegateHandler {

  def ui(linker: Linker) = {
    var dtabs = linker.routers.map { router =>
      val RoutingFactory.BaseDtab(dtab) = router.params[RoutingFactory.BaseDtab]
      router.label -> dtab()
    }.toMap
    new DelegateHandler(dtabs)
  }

  def api(linker: Linker) = new WebDelegator(linker)

  def render(dtab: Map[String, Dtab]) =
    AdminHandler.adminHtml(
      content = s"""
        <div class="row">
          <div class="col-lg-6">
            <h2 class="router-label-title">Router</h2>
            <div class="input-group path">
              <span class="input-group-addon" id="basic-addon1">Path</span>
              <input type="text" class="form-control" id="path-input" placeholder="/path/to/resource" aria-describedby="basic-addon1">
              <span class="input-group-btn">
                <button class="btn btn-default go" type="button">Go!</button>
              </span>
            </div>
          </div>
        </div>
        <div class="well well-sm dtab">
          <h4 class="header">
            Dtab
            <button id="edit-dtab-btn" class="btn btn-info btn-sm">Edit</button>
            <button id="save-dtab-btn" class="btn btn-success btn-sm hide disabled">Save</button>
          </h4>
          <div id="dtab"></div>
          <div id="dtab-edit" class="hide">
            <textarea type="text" class="form-control" id="dtab-input" rows="${dtab}" placeholder=""></textarea>
          </div>
          </div>
        </div>
        <div class="result">
        </div>
        <div class="modal fade error-modal" tabindex="-1" role="dialog">
        </div>
      """,
      tailContent = s"""
        <script id="data" type="application/json">${WebDelegator.Codec.writeStr(dtab)}</script>
      """,
      javaScripts = Seq("dtab_viewer.js", "delegate.js"),
      csses = Seq("delegator.css")
    )
}

private class DelegateHandler(dtab: Map[String, Dtab]) extends Service[Request, Response] {
  import DelegateHandler._

  lazy val dtabString = render(dtab)

  override def apply(req: Request): Future[Response] =
    AdminHandler.mkResponse(dtabString)
}
