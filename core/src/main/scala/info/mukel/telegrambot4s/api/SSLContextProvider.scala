package info.mukel.telegrambot4s.api

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

trait SSLContextProvider {
  def provide: SSLContext
}

object SSLContextProvider {
  def tlsFromPkcs12(path: File, passphrase: Array[Char]): SSLContextProvider = {
    val in = new BufferedInputStream(new FileInputStream(path))
    tlsFromPkcs12(in, passphrase)
  }

  def tlsFromPkcs12(source: InputStream, passphrase: Array[Char]): SSLContextProvider =
    new SSLContextProvider {
      override def provide: SSLContext = {
        val ks = KeyStore.getInstance("PKCS12")
        ks.load(source, passphrase)
        val kManFactory = KeyManagerFactory.getInstance("SunX509")
        kManFactory.init(ks, passphrase)
        val tmf = TrustManagerFactory.getInstance("SunX509")
        tmf.init(ks)

        val ctx = SSLContext.getInstance("TLS")
        ctx.init(kManFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom())
        ctx
      }
    }
}
